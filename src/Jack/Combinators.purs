module Jack.Combinators (
    noShrink
  , sized
  , resize
  , scale
  , chooseChar
  , chooseInt
  , boundedChar
  , boundedInt
  , frequency
  , elements
  , oneOf
  , oneOfRec
  , listOf
  , listOf1
  , listOfN
  , listOfN'
  , arrayOf
  , arrayOf1
  , arrayOfN
  , arrayOfN'
  , maybeOf
  , justOf
  , suchThat
  , suchThatMaybe
  ) where

import Control.Monad.Rec.Class (tailRecM2)

import Data.Array as Array
import Data.Char (toCharCode, fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..), fst)

import Jack.Gen (Gen(..), runGen, mkGen, mapTree, mapRandom)
import Jack.Random (Random, replicateRecM)
import Jack.Random as Random
import Jack.Shrink (shrinkTowards, sequenceShrinkList, sequenceShrinkOne)
import Jack.Tree (Tree(..), outcome, filterTree)

import Partial.Unsafe as Savage

import Prelude


-- | Prevent a 'Gen' from shrinking.
noShrink :: forall a. Gen a -> Gen a
noShrink =
  mapTree $ \(Node x _) ->
    Node x Lazy.nil

-- | Construct a 'Gen' that depends on the size parameter.
sized :: forall a. (Int -> Gen a) -> Gen a
sized f =
  Gen $ Random.sized (runGen <<< f)

-- | Overrides the size parameter. Returns a 'Gen' which uses the given size
--   instead of the runtime-size parameter.
resize :: forall a. Int -> Gen a -> Gen a
resize n =
  -- TODO throw when size is negative?
  mapRandom $ Random.resize n

-- | Update the current size by mapping a function over it.
scale :: forall a. (Int -> Int) -> Gen a -> Gen a
scale f j =
  sized $ \n ->
    resize (f n) j

-- | Generates a 'Char' in the given range.
chooseChar :: Char -> Char -> Gen Char
chooseChar x0 x1 =
  map fromCharCode $
    chooseInt (toCharCode x0) (toCharCode x1)

-- | Generates an integral number.
chooseInt :: Int -> Int -> Gen Int
chooseInt x0 x1 =
  let
    x_min =
      min x0 x1

    x_max =
      max x0 x1
  in
    mkGen (shrinkTowards x0) $
      Random.chooseInt x_min x_max

-- | Generates a 'Char'. The character is chosen from the entire range of valid
-- | 'Char' values, this is [0, 65535].
boundedChar :: Gen Char
boundedChar =
  chooseChar bottom top

-- | Generates an 'Int'. The number is chosen from the entire range of valid
-- | 'Int' values, this is [-2^31, 2^31).
boundedInt :: Gen Int
boundedInt =
  mkGen (shrinkTowards 0) $
    Random.chooseInt bottom top

-- | Uses a weighted distribution to randomly select one of the jacks in the array.
--   /The input array must be non-empty./
frequency :: forall a. Array (Tuple Int (Gen a)) -> Gen a
frequency xs =
  if Array.null xs then
    crashEmptyArray "frequency"
  else do
    let
      total =
        sum (map fst xs)

      pick n ys0 =
        case ys0 of
          Nil ->
            crashEmptyArray "frequency/pick"
          Cons (Tuple k y) ys ->
            if n <= k then
              y
            else
              pick (n - k) ys

    n <- chooseInt 1 total
    pick n $ List.fromFoldable xs

-- | Randomly selects one of the values in the array.
-- | /The input array must be non-empty./
elements :: forall a. Array a -> Gen a
elements xs =
  let
    fromIx :: Int -> a
    fromIx ix =
      case Array.index xs ix of
        Nothing ->
          crashEmptyArray "elements"
        Just x ->
          x
  in
    if Array.null xs then
      crashEmptyArray "elements"
    else do
      fromIx <$> chooseInt 0 (Array.length xs - 1)

-- | Randomly selects one of the jacks in the array.
-- | /The input array must be non-empty./
oneOf :: forall a. Array (Gen a) -> Gen a
oneOf xs =
  let
    fromIx :: Int -> Gen a
    fromIx ix =
      case Array.index xs ix of
        Nothing ->
          crashEmptyArray "oneOf"
        Just x ->
          x
  in
    if Array.null xs then
      crashEmptyArray "oneOf"
    else do
      fromIx =<< chooseInt 0 (Array.length xs - 1)

-- | Randomly selects from one of the jacks in either the non-recursive or the
-- | recursive array. When a selection is made from the recursive array, the size
-- | is halved. When the size gets to one or less, selections are no longer made
-- | from the recursive array.
-- | /The first argument (i.e. the non-recursive input array) must be non-empty./
oneOfRec :: forall a. Array (Gen a) -> Array (Gen a) -> Gen a
oneOfRec nonrec rec =
  sized $ \n ->
    if n <= 1 then
      oneOf nonrec
    else
      oneOf $ nonrec <> map (scale (_ `div` 2)) rec

-- | Generates a list of random length. The maximum length depends on the size
-- | parameter.
listOf :: forall a. Gen a -> Gen (List a)
listOf (Gen g) =
  sized $ \n ->
    Gen $ do
      k <- Random.chooseInt 0 n
      xs <- replicateRecM k g
      pure $ sequenceShrinkList xs

-- | Generates a non-empty list of random length. The maximum length depends on
-- | the size parameter.
listOf1 :: forall a. Gen a -> Gen (NonEmpty List a)
listOf1 jack =
  sized $ \n -> do
    Gen $ do
      k <- Random.chooseInt 1 (max n 1)

      let
        unpack ys0 =
          case ys0 of
            Nil ->
              Savage.unsafeCrashWith $
                "Jack.Combinators.listOf1: " <>
                "internal error, generated empty list"
            Cons y ys ->
              NonEmpty y ys

        go =
          map unpack <<<
          filterTree (not <<< List.null) <<<
          sequenceShrinkList

      map go <<< Random.replicateRecM k $ runGen jack

-- | Generates a list of the given length.
listOfN :: forall a. Int -> Gen a -> Gen (List a)
listOfN n =
  mapRandom (map sequenceShrinkOne <<< Random.replicateRecM n)

-- | Generates a list between 'n' and 'm' in length.
listOfN' :: forall a. Int -> Int -> Gen a -> Gen (List a)
listOfN' n m (Gen r) =
  Gen $ do
    k <- Random.chooseInt n m

    let
      k_min =
        min n m

      check xs =
        List.length xs >= k_min

    map (filterTree check <<< sequenceShrinkList) $
      Random.replicateRecM k r

-- | Generates an array of random length. The maximum length depends on the
-- | size parameter.
arrayOf :: forall a. Gen a -> Gen (Array a)
arrayOf =
  map Array.fromFoldable <<< listOf

-- | Generates a non-empty array of random length. The maximum length depends
-- | on the size parameter.
arrayOf1 :: forall a. Gen a -> Gen (NonEmpty Array a)
arrayOf1 =
  map (transNonEmpty Array.fromFoldable) <<< listOf1

-- | Generates an array of the given length.
arrayOfN :: forall a. Int -> Gen a -> Gen (Array a)
arrayOfN n =
  map Array.fromFoldable <<< listOfN n

-- | Generates an array between 'n' and 'm' in length.
arrayOfN' :: forall a. Int -> Int -> Gen a -> Gen (Array a)
arrayOfN' n m =
  map Array.fromFoldable <<< listOfN' n m

-- | Generates a 'Nothing' some of the time.
maybeOf :: forall a. Gen a -> Gen (Maybe a)
maybeOf jack =
  sized $ \n ->
    frequency [
        Tuple 2 $
          pure Nothing
      , Tuple (1 + n) $
          Just <$> jack
      ]

-- | Runs a generator that produces 'Maybe a' until it produces a 'Just'.
justOf :: forall a. Gen (Maybe a) -> Gen a
justOf g = do
  mx <- suchThat g isJust
  case mx of
    Just x ->
      pure x
    Nothing ->
      Savage.unsafeCrashWith $
        "Jack.Combinators.justOf: " <>
        "internal error, unexpected Nothing"

-- | Generates a value that satisfies a predicate.
suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
suchThat (Gen r) p =
  Gen $
    let
      -- TODO this might blow the stack
      loop = do
        mx <- tryRandom r p
        case mx of
          Just x ->
            pure x
          Nothing ->
            Random.sized $ \n ->
              Random.resize (n + 1) loop
    in
      loop

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: forall a. Gen a -> (a -> Boolean) -> Gen (Maybe a)
suchThatMaybe (Gen r) p =
  Gen $ do
    mx <- tryRandom r p
    case mx of
      Nothing ->
        pure $ pure Nothing
      Just x ->
        pure $ map Just x

-- | More or less the same logic as suchThatMaybe from QuickCheck, except
-- | modified to ensure that the shrinks also obey the predicate.
tryRandom :: forall a. Random (Tree a) -> (a -> Boolean) -> Random (Maybe (Tree a))
tryRandom r p =
  let
    try k n =
      case n of
        0 ->
          pure $ Right Nothing
        _ ->
          Random.resize (2 * k + n) r >>= \x ->
            if p (outcome x) then
              pure <<< Right <<< Just $ filterTree p x
            else
              pure $ Left { a: k + 1, b: n - 1 }
  in
    Random.sized $ tailRecM2 try 0 <<< max 1

transNonEmpty :: forall f g a. (f a -> g a) -> NonEmpty f a -> NonEmpty g a
transNonEmpty f (NonEmpty x xs) =
  NonEmpty x $ f xs

crashEmptyArray :: forall a. String -> a
crashEmptyArray fn =
  Savage.unsafeCrashWith $
    "Jack.Combinators." <> fn <> ": used with empty array"
