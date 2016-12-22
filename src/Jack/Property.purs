module Jack.Property (
    Result(..)
  , renderResult

  , Property
  , mkProperty
  , unProperty
  , property

  , check
  , check'
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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (tailRec)

import Data.Either (Either(..))
import Data.Foldable (for_, foldMap, intercalate)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))

import Jack.Gen (Gen(..), runGen)
import Jack.Random (Size, runRandom, replicateRecM)
import Jack.Seed (randomSeed, splitSeed)
import Jack.Tree (Tree(..), outcome, shrinks)

import Prelude


data Result =
    Success
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

newtype Property =
  Property { "Property :: Gen Result" :: Gen Result }

mkProperty :: Gen Result -> Property
mkProperty gen =
  Property { "Property :: Gen Result": gen }

unProperty :: Property -> Gen Result
unProperty (Property x) =
  x."Property :: Gen Result"

mapGen :: (Gen Result -> Gen Result) -> Property -> Property
mapGen f =
  mkProperty <<< f <<< unProperty

property :: Boolean -> Property
property b =
  if b then
    mkProperty $ pure Success
  else
    mkProperty <<< pure $ Failure List.Nil

counterexample :: String -> Property -> Property
counterexample msg =
  mapGen <<< map <<< mapFailure $ Cons msg

forAll :: forall a. Show a => Gen a -> (a -> Property) -> Property
forAll =
  forAllRender show

forAllRender :: forall a. (a -> String) -> Gen a -> (a -> Property) -> Property
forAllRender render gen f =
  let
    prepend x =
      unProperty $ counterexample (render x) (f x)
  in
    mkProperty $ bind gen prepend

check :: forall e. Property -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
check =
  check' 100

check' :: forall e. Int -> Property -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
check' n p = do
  let
    random =
        runGen $ unProperty p

    nextSize size =
      if size >= 100 then
        1
      else
        size + 1

    loop { seed, size, tests } =
      if tests == n then
        Right {
            tests
          , result: pure Success
          }
      else
        case splitSeed seed of
          Tuple seed1 seed2 ->
            let
              result =
                runRandom seed1 size random
            in
              case outcome result of
                Failure _ ->
                  Right {
                      tests: tests + 1
                    , result
                    }

                Success ->
                  Left {
                      seed: seed2
                    , size: nextSize size
                    , tests: tests + 1
                    }

  seed <- randomSeed

  let
    x =
      tailRec loop { seed, size: 1, tests: 0 }

  case takeSmallest x.result 0 of
    Nothing -> do
      log $ "+++ OK, passed " <> renderTests x.tests <> "."
      pure true
    Just { nshrinks, msgs } -> do
      log $ "*** Failed! Falsifiable (after " <>
        renderTests x.tests <> renderShrinks nshrinks <> "):"
      log $ renderResult $ Failure msgs
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

takeSmallest :: Tree Result -> Int -> Maybe { nshrinks :: Int, msgs :: List String }
takeSmallest (Node x xs) nshrinks =
  case x of
    Success ->
      Nothing
    Failure msgs ->
      case firstFailure xs of
        Nothing ->
          Just { nshrinks, msgs }
        Just tree ->
          takeSmallest tree (nshrinks + 1)

takeFailure :: Tree Result -> Maybe (Tree Result)
takeFailure t@(Node x _) =
  case x of
    Success ->
      Nothing
    Failure _ ->
      Just t

firstFailure :: Lazy.List (Tree Result) -> Maybe (Tree Result)
firstFailure =
  runFirst <<< foldMap (First <<< takeFailure)

-- | Generate some example trees.
sampleTree :: forall e a. Size -> Int -> Gen a -> Eff ("random" :: RANDOM | e) (List (Tree a))
sampleTree size count (Gen r) = do
  seed <- randomSeed
  pure <<< runRandom seed size $
    replicateRecM count r

-- | Generate some example outcomes (and shrinks) and prints them to 'stdout'.
printSample :: forall e a. Show a => Gen a -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
printSample gen = do
  forest <- map (List.take 5) $ sampleTree 10 5 gen
  for_ forest $ \tree -> do
    log "=== Outcome ==="
    log <<< show $ outcome tree
    log "=== Shrinks ==="
    traverse_ (log <<< show <<< outcome) $ shrinks tree
    log ""

printSampleTree :: forall e a. Show a => Gen a -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
printSampleTree gen = do
  forest <- map (List.take 1) $ sampleTree 10 1 gen
  for_ forest $ \tree -> do
    log $ show tree

assertEq :: forall a. (Eq a, Show a) => a -> a -> Property
assertEq x y =
  let
    render a b = show a <> " /= " <> show b
  in
   counterexample "=== Not equal ===" $
   counterexample (render x y) (property (x == y))

assertNotEq :: forall a. (Eq a, Show a) => a -> a -> Property
assertNotEq x y =
  let
    render a b = show a <> " == " <> show b
  in
   counterexample "=== Equal ===" $
   counterexample (render x y) (property (x /= y))

infix 4 assertEq as ===

infix 4 assertNotEq as =/=
