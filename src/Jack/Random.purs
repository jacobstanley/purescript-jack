module Jack.Random
  ( Size
  , Random(..)
  , runRandom

  , sized
  , resize

  , chooseInt

  -- ** Unsafe
  , unsafeChooseInt53

  -- ** Utils
  , replicateRecM
  ) where

import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)

import Data.Int53 (Int53)
import Data.Int53 as Int53
import Data.List (List(..))
import Data.Tuple (Tuple(..), fst)

import Jack.Seed (Seed, splitSeed, nextInt53)

import Prelude

-- | Tests are parameterized by the size of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size =
  Int

--- | A generator for random values of type @a@.
newtype Random a =
  Random (Seed -> Size -> a)

-- | Run a random generator.
runRandom :: forall a. Seed -> Size -> Random a -> a
runRandom seed size (Random r) =
  r seed size

-- | Used to construct generators that depend on the size parameter.
sized :: forall a. (Size -> Random a) -> Random a
sized f =
  Random $ \seed size ->
    runRandom seed size (f size)

-- | Overrides the size parameter. Returns a generator which uses the
-- | given size instead of the runtime-size parameter.
resize :: forall a. Size -> Random a -> Random a
resize newSize r =
  Random $ \seed _ ->
    runRandom seed (max 1 newSize) r

-- | /This is not safe when (hi - lo) > 53-bits/
unsafeChooseInt53 :: Int53 -> Int53 -> Random Int53
unsafeChooseInt53 lo hi =
  Random $ \seed _ ->
    fst $ nextInt53 lo hi seed

-- | Generates a random element in the given inclusive range.
chooseInt :: Int -> Int -> Random Int
chooseInt lo hi =
  map Int53.toInt $
    unsafeChooseInt53 (Int53.fromInt lo) (Int53.fromInt hi)

-- | Tail recursive replicate.
replicateRecM :: forall m a. MonadRec m => Int -> m a -> m (List a)
replicateRecM k m =
  let
    go { acc, n } =
      if n <= 0 then
        pure $ Done acc
      else
        map (\x -> Loop { acc: Cons x acc, n: n - 1 }) m
  in
    tailRecM go { acc: Nil, n: k }

------------------------------------------------------------------------
-- Instances

instance functorRandom :: Functor Random where
  map f r =
    Random $ \seed size ->
      f (runRandom seed size r)

instance applyRandom :: Apply Random where
  apply =
    ap

instance applicativeRandom :: Applicative Random where
  pure x =
    Random $ \_ _ ->
      x

instance bindRandom :: Bind Random where
  bind r k =
    Random $ \seed size ->
      case splitSeed seed of
        Tuple seed1 seed2 ->
          runRandom seed2 size <<< k $
            runRandom seed1 size r

instance monadRandom :: Monad Random

instance monadRecRandom :: MonadRec Random where
  tailRecM k a0 =
    let
      go { seed, size, a } =
        case splitSeed seed of
          Tuple seed1 seed2 ->
            case runRandom seed1 size $ k a of
              Loop a1 ->
                Loop { seed: seed2, size, a: a1 }
              Done b ->
                Done b
    in
      Random $ \seed size ->
        tailRec go { seed, size, a: a0 }

instance lazyRandom :: Lazy (Random a) where
  defer f =
    Random $ \seed size ->
      runRandom seed size $ f unit
