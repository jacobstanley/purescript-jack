module Jack.Gen
  ( Gen(..)
  , runGen
  , mkGen
  , mkGen_
  , mapRandom
  , mapTree
  , reshrink
  , reshrinkLazy
  ) where

import Control.Lazy (class Lazy, defer)

import Data.List.Lazy as Lazy
import Data.Tuple (Tuple(..))

import Jack.Random (Random(..), runRandom)
import Jack.Seed (Seed, splitSeed)
import Jack.Tree (Tree, unfoldTree, expandTree)

import Prelude

-- | A generator for random values of type @a@ that includes all the possible
-- | shrink scenarios for @a@.
newtype Gen a =
  Gen (Random (Tree a))

runGen :: forall a. Gen a -> Random (Tree a)
runGen (Gen gen) =
  gen

-- | Create a 'Gen' from a shrink function and a 'Random'.
mkGen :: forall a. (a -> Lazy.List a) -> Random a -> Gen a
mkGen shr =
  Gen <<< map (unfoldTree identity shr)

-- | Create a non-shrinking 'Gen' from a 'Random'.
mkGen_ :: forall a. Random a -> Gen a
mkGen_ =
  mkGen $ const Lazy.nil

-- | Map over the 'Random' inside of 'Gen'.
mapRandom :: forall a b. (Random (Tree a) -> Random (Tree b)) -> Gen a -> Gen b
mapRandom f =
  Gen <<< f <<< runGen

-- | Map over the 'Tree' inside a 'Gen'.
mapTree :: forall a b. (Tree a -> Tree b) -> Gen a -> Gen b
mapTree =
  mapRandom <<< map

-- | Apply an additional shrinker to all generated trees.
reshrink :: forall a. (a -> Array a) -> Gen a -> Gen a
reshrink f =
  reshrinkLazy (Lazy.fromFoldable <<< f)

-- | Apply an additional shrinker to all generated trees.
reshrinkLazy :: forall a. (a -> Lazy.List a) -> Gen a -> Gen a
reshrinkLazy =
  mapTree <<< expandTree

------------------------------------------------------------------------
-- Instances

instance functorGen :: Functor Gen where
  map f =
    Gen <<< map (map f) <<< runGen

instance applyGen :: Apply Gen where
  apply f x =
    Gen $
      (<*>) <$> runGen f <*> runGen x

instance applicativeGen :: Applicative Gen where
  pure =
    Gen <<< pure <<< pure

instance bindGen :: Bind Gen where
  bind m0 k0 =
    let
      bindRandom :: forall a b. Random (Tree a) -> (a -> Random (Tree b)) -> Random (Tree b)
      bindRandom m k =
        Random \seed0 size ->
          case splitSeed seed0 of
            Tuple seed1 seed2 ->
              let
                run :: forall x. Seed -> Random x -> x
                run seed random =
                  runRandom seed size random
              in
                bind (run seed1 m) (run seed2 <<< k)
    in
      Gen $ bindRandom (runGen m0) (runGen <<< k0)

instance monadGen :: Monad Gen

instance lazyGen :: Lazy (Gen a) where
  defer f =
    Gen $ defer (runGen <<< f)
