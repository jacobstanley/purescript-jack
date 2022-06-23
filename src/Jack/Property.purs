module Jack.Property
  ( Result(..)
  , renderResult

  , Property
  , Property'
  , mkProperty
  , unProperty
  , property
  , propertyM
  , resultM

  , check
  , check'
  , checkM
  , checkM'

  , forAll
  , forAllRender
  , counterexample
  , assertEq
  , assertNotEq
  , (===)
  , (=/=)

  , sampleTree
  , printSample
  , printSampleTree
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Foldable (for_, intercalate)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Jack.Gen (Gen(..), runGen)
import Jack.Random (Random, Size, replicateRecM, runRandom)
import Jack.Seed (randomSeed, splitSeed)
import Jack.Tree (Tree(..), outcome, shrinks)

data Result
  = Success
  | Failure (List String)

instance showResult :: Show Result where
  show xx =
    case xx of
      Success ->
        "Success"
      Failure xs ->
        "(Failure " <> show xs <> ")"

renderResult :: Result -> String
renderResult xx =
  case xx of
    Success ->
      "Success"
    Failure xs ->
      intercalate "\n" xs

mapFailure :: (List String -> List String) -> Result -> Result
mapFailure f xx =
  case xx of
    Success ->
      Success
    Failure xs ->
      Failure $ f xs

type Property = Property' Identity

newtype Property' m =
  Property { "Property :: Gen Result" :: Gen (m Result) }

mkProperty :: forall m. Applicative m => Gen Result -> Property' m
mkProperty = mkProperty' <<< map pure

unProperty :: Property -> Gen Result
unProperty = map unwrap <<< unProperty'

mkProperty' :: forall m. Gen (m Result) -> Property' m
mkProperty' gen =
  Property { "Property :: Gen Result": gen }

unProperty' :: forall m. Property' m -> Gen (m Result)
unProperty' (Property x) =
  x."Property :: Gen Result"

hoistProperty :: forall m n. (forall a. m a -> n a) -> Property' m -> Property' n
hoistProperty f = mapGen (map f)

generalize :: forall m a. Applicative m => Identity a -> m a
generalize (Identity x) = pure x

mapGen :: forall m n. (Gen (m Result) -> Gen (n Result)) -> Property' m -> Property' n
mapGen f =
  mkProperty' <<< f <<< unProperty'

mapResult :: forall m. Functor m => (Result -> Result) -> Property' m -> Property' m
mapResult =
  mapGen <<< map <<< map

property :: forall m. Applicative m => Boolean -> Property' m
property b =
  if b then
    mkProperty $ pure Success
  else
    mkProperty <<< pure $ Failure List.Nil

propertyM :: forall m. Monad m => m Boolean -> Property' m
propertyM mb =
  resultM $ do
    b <- mb
    if b then pure Success
    else pure $ Failure List.Nil

resultM :: forall m. m Result -> Property' m
resultM = mkProperty' <<< pure

counterexample :: forall m. Functor m => String -> Property' m -> Property' m
counterexample msg =
  mapResult <<< mapFailure $ Cons msg

forAll :: forall a m. Monad m => Show a => Gen a -> (a -> Property' m) -> Property' m
forAll =
  forAllRender show

forAllRender :: forall a m. Monad m => (a -> String) -> Gen a -> (a -> Property' m) -> Property' m
forAllRender render gen f =
  let
    prepend x =
      unProperty' $ counterexample (render x) (f x)
  in
    mkProperty' $ bind gen prepend

check :: Property -> Effect Boolean
check =
  check' 100

check' :: Int -> Property -> Effect Boolean
check' n = checkM' n <<< hoistProperty generalize

checkM :: forall m. MonadRec m => MonadEffect m => Property' m -> m Boolean
checkM = checkM' 100

checkM' :: forall m. MonadRec m => MonadEffect m => Int -> Property' m -> m Boolean
checkM' n p = do
  let
    random :: Random (Tree (m Result))
    random =
      runGen $ unProperty' p

    nextSize size =
      if size >= 100 then
        1
      else
        size + 1

    loop { seed, size, tests } =
      if tests == n then
        pure $ Done
          { tests
          , result: pure (pure Success)
          }
      else do
        let
          Tuple seed1 seed2 = splitSeed seed
          result = runRandom seed1 size random
        outcome' <- outcome result
        pure $ case outcome' of
          Failure _ ->
            Done
              { tests: tests + 1
              , result: Node (pure outcome') (shrinks result)
              }

          Success ->
            Loop
              { seed: seed2
              , size: nextSize size
              , tests: tests + 1
              }

  seed <- liftEffect randomSeed

  x <- tailRecM loop { seed, size: 1, tests: 0 }

  smallest <- takeSmallest x.result 0
  case smallest of
    Nothing -> do
      liftEffect $ log $ "+++ OK, passed " <> renderTests x.tests <> "."
      pure true
    Just { nshrinks, msgs } -> do
      liftEffect $ log $ "*** Failed! Falsifiable (after "
        <> renderTests x.tests
        <> renderShrinks nshrinks
        <> "):"
      liftEffect $ log $ renderResult $ Failure msgs
      pure false

renderTests :: Int -> String
renderTests n =
  case n of
    1 ->
      "1 test"
    _ ->
      show n <> " tests"

renderShrinks :: Int -> String
renderShrinks n =
  case n of
    0 ->
      ""
    1 ->
      " and 1 shrink"
    _ ->
      " and " <> show n <> " shrinks"

takeSmallest :: forall m. Monad m => Tree (m Result) -> Int -> m (Maybe { nshrinks :: Int, msgs :: List String })
takeSmallest (Node x xs) nshrinks = do
  outcome' <- x
  case outcome' of
    Success ->
      pure Nothing
    Failure msgs -> do
      firstFailure' <- firstFailure xs
      case firstFailure' of
        Nothing ->
          pure $ Just { nshrinks, msgs }
        Just tree ->
          takeSmallest tree (nshrinks + 1)

takeFailure :: Tree Result -> Maybe (Tree Result)
takeFailure t@(Node x _) =
  case x of
    Success ->
      Nothing
    Failure _ ->
      Just t

firstFailure :: forall m. Monad m => Lazy.List (Tree (m Result)) -> m (Maybe (Tree (m Result)))
firstFailure l =
  case Lazy.step l of
    Lazy.Nil -> pure Nothing
    Lazy.Cons x xs -> do
      outcome' <- outcome x
      case outcome' of
        Success ->
          firstFailure xs
        Failure _ ->
          pure $ Just $ Node (pure outcome') (shrinks x)

-- | Generate some example trees.
sampleTree :: forall a. Size -> Int -> Gen a -> Effect (List (Tree a))
sampleTree size count (Gen r) = do
  seed <- randomSeed
  pure <<< runRandom seed size $
    replicateRecM count r

-- | Generate some example outcomes (and shrinks) and prints them to 'stdout'.
printSample :: forall a. Show a => Gen a -> Effect Unit
printSample gen = do
  forest <- map (List.take 5) $ sampleTree 10 5 gen
  for_ forest $ \tree -> do
    log "=== Outcome ==="
    log <<< show $ outcome tree
    log "=== Shrinks ==="
    traverse_ (log <<< show <<< outcome) $ shrinks tree
    log ""

printSampleTree :: forall a. Show a => Gen a -> Effect Unit
printSampleTree gen = do
  forest <- map (List.take 1) $ sampleTree 10 1 gen
  for_ forest $ \tree -> do
    log $ show tree

assertEq :: forall m a. Applicative m => Eq a => Show a => a -> a -> Property' m
assertEq x y =
  let
    render a b = show a <> " /= " <> show b
  in
    counterexample "=== Not equal ===" $
      counterexample (render x y) (property (x == y))

assertNotEq :: forall m a. Applicative m => Eq a => Show a => a -> a -> Property' m
assertNotEq x y =
  let
    render a b = show a <> " == " <> show b
  in
    counterexample "=== Equal ===" $
      counterexample (render x y) (property (x /= y))

infix 4 assertEq as ===

infix 4 assertNotEq as =/=
