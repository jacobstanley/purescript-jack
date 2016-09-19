module Jack.Gen where

import Control.Lazy (class Lazy, defer)

import Data.List.Lazy as Lazy
import Data.Traversable (traverse)

import Jack.Random (Random, unsafePromote)
import Jack.Tree (Tree, unfoldTree, expandTree)

import Prelude


newtype Gen a =
  Gen (Random (Tree a))

runGen :: forall a. Gen a -> Random (Tree a)
runGen (Gen gen) =
  gen

-- | Create a 'Gen' from a shrink function and a 'Random'.
mkGen :: forall a. (a -> Lazy.List a) -> Random a -> Gen a
mkGen shr =
  Gen <<< map (unfoldTree id shr)

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
      go :: forall a b. Random (Tree a) -> (a -> Random (Tree b)) -> Random (Tree b)
      go m k =
        bind m \ta ->
          map join <<< unsafePromote $ map k ta
    in
      Gen $ go (runGen m0) (runGen <<< k0)

instance monadGen :: Monad Gen

instance lazyGen :: Lazy (Gen a) where
  defer f =
    Gen $ defer (runGen <<< f)
