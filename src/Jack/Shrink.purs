module Jack.Shrink where

import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy

import Jack.Tree (Tree(..), outcome, shrinks)

import Prelude

-- | Shrink an integral by edging towards a destination number.
shrinkTowards :: forall a. (Ord a, EuclideanRing a) => a -> a -> Lazy.List a
shrinkTowards destination x =
  if destination == x then
    Lazy.nil
  else
    let
      two =
        one + one

      -- We need to halve our operands before subtracting them as they may be using
      -- the full range of the type (i.e. 'minBound' and 'maxBound' for 'Int32')
      diff =
        (x `div` two) - (destination `div` two)
    in
      -- We make up for halving the inputs by explicitly prepending the
      -- destination as the first element of the list.
      destination `consNub` map (\y -> x - y) (halves diff)

consNub :: forall a. Eq a => a -> Lazy.List a -> Lazy.List a
consNub x ys0 =
  case Lazy.step ys0 of
    Lazy.Nil ->
      Lazy.singleton x
    Lazy.Cons y ys ->
      if x == y then
        Lazy.cons y ys
      else
        Lazy.cons x $ Lazy.cons y ys

-- | Turn a list of trees in to a tree of lists, opting to shrink only the
-- | elements of the list (i.e. the size of the list will always be the same).
sequenceShrinkOne :: forall a. List (Tree a) -> Tree (List a)
sequenceShrinkOne =
  sequenceShrink (\xs -> shrinkOne shrinks xs)

-- | Turn a list of trees in to a tree of lists, opting to shrink both the list
-- | itself and the elements in the list during traversal.
sequenceShrinkList :: forall a. List (Tree a) -> Tree (List a)
sequenceShrinkList =
  sequenceShrink (\xs -> shrinkList xs <> shrinkOne shrinks xs)

-- | Turn a list of trees in to a tree of lists, using the supplied function to
-- | merge shrinking options.
sequenceShrink ::
  forall a.
  (List (Tree a) -> Lazy.List (List (Tree a))) ->
  List (Tree a) ->
  Tree (List a)
sequenceShrink merge xs =
  Node
    (map outcome xs)
    (map (sequenceShrink merge) $ merge xs)

-- | Shrink each of the elements in input list using the supplied shrinking
-- | function.
shrinkOne :: forall a. (a -> Lazy.List a) -> List a -> Lazy.List (List a)
shrinkOne shr xs00 =
  case xs00 of
    Nil ->
      Lazy.nil
    Cons x0 xs0 ->
      -- TODO refactor, was list comprehension
      (do x1 <- shr x0
          pure $ Cons x1 xs0) <>
      (do xs1 <- shrinkOne shr xs0
          pure $ Cons x0 xs1)

-- | Produce a smaller permutation of the input list.
shrinkList :: forall a. List a -> Lazy.List (List a)
shrinkList xs = do
 Lazy.concatMap
   (\k -> removes k xs)
   (halves $ List.length xs)

-- | Produces a list containing the results of halving a number over and over
-- | again.
-- |
-- | > halves 30 == [30,15,7,3,1]
-- | > halves 128 == [128,64,32,16,8,4,2,1]
-- | > halves (-10) == [-10,-5,-2,-1]
-- |
halves :: forall a. (Ord a, EuclideanRing a) => a -> Lazy.List a
halves =
  let
    two =
      one + one
  in
    Lazy.takeWhile (\x -> x /= zero) <<<
    Lazy.iterate (\x -> x `div` two)

-- | Permutes a list by removing 'k' consecutive elements from it:
-- |
-- | > removes 2 [1,2,3,4,5,6] == [[3,4,5,6],[1,2,5,6],[1,2,3,4]]
-- |
removes :: forall a. Int -> List a -> Lazy.List (List a)
removes k0 xs0 =
  let
    loop :: Int -> Int -> List a -> Lazy.List (List a)
    loop k n xs =
      let
        hd = List.take k xs
        tl = List.drop k xs
      in
        if k > n then
          Lazy.nil
        else if List.null tl then
          Lazy.singleton Nil
        else
          Lazy.cons tl $ map (\x -> hd <> x) (loop k (n - k) tl)
  in
    loop k0 (List.length xs0) xs0
