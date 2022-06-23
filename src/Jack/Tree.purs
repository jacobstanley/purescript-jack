module Jack.Tree
  ( Tree(..)
  , outcome
  , shrinks
  , foldTree
  , foldForest
  , unfoldTree
  , unfoldForest
  , expandTree
  , filterTree
  , filterForest
  ) where

import Control.Extend (class Extend)
import Control.Comonad (class Comonad)

import Data.Array as Array
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.List.Lazy as Lazy
import Data.Traversable (class Traversable, traverse, sequence)

import Prelude

-- | A rose tree which represents a random generated outcome, and all the ways
-- | in which it can be made smaller.
-- |
-- | This tree is exactly the same as 'Data.Tree' in every way except that
-- | Applicative '<*>' and Monad '>>=' walk the tree in the reverse order. This
-- | modification is critical for shrinking to reach a minimal counterexample.
data Tree a =
  Node a (Lazy.List (Tree a))

instance showTree :: Show a => Show (Tree a) where
  show (Node x xs) =
    "(Node " <> show x <> " " <> show (Array.fromFoldable xs) <> ")"

-- | The generated outcome.
outcome :: forall a. Tree a -> a
outcome (Node x _) =
  x

-- | All the possible shrinks of this outcome. This should be ordered
-- | smallest to largest as if property still fails with the first shrink in
-- | the list then we will commit to that path and none of the others will
-- | be tried (i.e. there is no backtracking).
shrinks :: forall a. Tree a -> Lazy.List (Tree a)
shrinks (Node _ xs) =
  xs

instance functorTree :: Functor Tree where
  map f (Node x xs) =
    Node (f x) $ map (map f) xs

instance foldableTree :: Foldable Tree where
  foldr o b (Node x xs) =
    x `o` foldr (flip $ foldr o) b xs

  foldl o b (Node x xs) =
    foldl (foldl o) (b `o` x) xs

  foldMap f (Node x xs) =
    f x <> foldMap (foldMap f) xs

instance traversableTree :: Traversable Tree where
  traverse f (Node x xs) =
    Node <$> f x <*> traverse (traverse f) xs

  sequence (Node x xs) =
    Node <$> x <*> traverse sequence xs

instance applicativeTree :: Applicative Tree where
  pure x =
    Node x Lazy.nil

instance applyTree :: Apply Tree where
  apply (Node f fs) x@(Node y ys) =
    Node (f y) $
      -- Data.Tree would have:
      --   map (map f) ys <>
      --   map (flip apply x) fs
      map (flip apply x) fs <>
        map (map f) ys

instance bindTree :: Bind Tree where
  bind (Node x xs) k =
    case k x of
      Node y ys ->
        Node y $
          -- Data.Tree would have: ys <> map (flip bind k) xs
          map (flip bind k) xs <> ys

instance monadTree :: Monad Tree

instance extendTree :: Extend Tree where
  extend f =
    map f <<< duplicateTree

-- | Comonad duplicate for a 'Tree'.
duplicateTree :: forall a. Tree a -> Tree (Tree a)
duplicateTree x@(Node _ ys) =
  Node x (map duplicateTree ys)

instance comonadTree :: Comonad Tree where
  extract (Node x _) =
    x

-- | Fold over a 'Tree'.
foldTree :: forall a b x. (a -> x -> b) -> (Lazy.List b -> x) -> Tree a -> b
foldTree f g (Node x xs) =
  f x (foldForest f g xs)

-- | Fold over a list of trees.
foldForest :: forall a b x. (a -> x -> b) -> (Lazy.List b -> x) -> Lazy.List (Tree a) -> x
foldForest f g =
  g <<< map (foldTree f g)

-- | Build a 'Tree' from an unfolding function and a seed value.
unfoldTree :: forall a b. (b -> a) -> (b -> Lazy.List b) -> b -> Tree a
unfoldTree f g x =
  Node (f x) (unfoldForest f g x)

-- | Build a list of trees from an unfolding function and a seed value.
unfoldForest :: forall a b. (b -> a) -> (b -> Lazy.List b) -> b -> Lazy.List (Tree a)
unfoldForest f g =
  map (unfoldTree f g) <<< g

-- | Apply an additional unfolding function to an existing tree.
-- |
-- | The root outcome remains intact, only the shrinks are affected, this
-- | applies recursively, so shrinks can only ever be added using this
-- | function.
-- |
-- | If you want to replace the shrinks altogether, try:
-- |
-- | > unfoldTree f (outcome oldTree)
-- |
expandTree :: forall a. (a -> Lazy.List a) -> Tree a -> Tree a
expandTree f (Node x xs) =
  --
  -- Ideally we could put the 'unfoldForest' nodes before the 'fmap expandTree'
  -- nodes, so that we're culling from the top down and we would be able to
  -- terminate our search faster, but this prevents minimal shrinking.
  --
  -- We'd need some kind of tree transpose to do this properly.
  --
  Node x (map (expandTree f) xs <> unfoldForest identity f x)

-- | Recursively discard any shrinks whose outcome does not pass the predicate.
-- | /Note that the root outcome can never be discarded./
filterTree :: forall a. (a -> Boolean) -> Tree a -> Tree a
filterTree f (Node x xs) =
  Node x (filterForest f xs)

-- | Recursively discard any trees whose outcome does not pass the predicate.
filterForest :: forall a. (a -> Boolean) -> Lazy.List (Tree a) -> Lazy.List (Tree a)
filterForest f =
  map (filterTree f) <<< Lazy.filter (f <<< outcome)
