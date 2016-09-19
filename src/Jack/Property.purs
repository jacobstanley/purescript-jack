module Jack.Property where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (tailRecM, tailRec)

import Data.Array as Array
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

-- | Generate some example trees.
sampleTree :: forall e a. Size -> Int -> Gen a -> Eff ("random" :: RANDOM | e) (List (Tree a))
sampleTree size count (Gen r) = do
  seed <- randomSeed
  pure <<< runRandom seed size $
    replicateRecM count r

-- | Generate some example outcomes (and shrinks) and prints them to 'stdout'.
printSample :: forall e a. Show a => Gen a -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
printSample gen = do
  forest <- map (List.take 5) $ sampleTree 10 10 gen
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
      "Failure:\n" <>
      intercalate "\n" (map ("  " <> _) xs)

mapFailure :: (List String -> List String) -> Result -> Result
mapFailure f xx =
  case xx of
    Success ->
      Success
    Failure xs ->
      Failure $ f xs

property :: Boolean -> Gen Result
property b =
  if b then
    pure Success
  else
    pure $ Failure List.Nil

counterexample :: String -> Gen Result -> Gen Result
counterexample msg =
  map (mapFailure $ Cons msg)

forAll :: forall a. Show a => Gen a -> (a -> Gen Result) -> Gen Result
forAll jack f =
  let
    prepend x =
      counterexample (show x) (f x)
  in
    bind jack prepend

check :: forall e. Gen Result -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
check =
  check' 100

check' :: forall e. Int -> Gen Result -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
check' n (Gen r) = do
  let
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
                runRandom seed1 size r
            in
              case outcome result of
                Failure _ ->
                  Right {
                      tests
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

  case takeSmallest x.result of
    Nothing -> do
      log $ show x.tests <> " test(s) passed."
      pure true
    Just msgs -> do
      log $ renderResult $ Failure msgs
      pure false

takeSmallest :: Tree Result -> Maybe (List String)
takeSmallest (Node x xs) =
  case x of
    Success ->
      Nothing
    Failure msgs ->
      case firstFailure xs of
        Nothing ->
          Just msgs
        Just tree ->
          takeSmallest tree

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
