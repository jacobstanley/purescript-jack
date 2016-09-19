--
-- This is a port of GHC's System.Random implementation.
--
-- This implementation uses the Portable Combined Generator of L'Ecuyer for
-- 32-bit computers [1], transliterated by Lennart Augustsson.
--
-- 1. Pierre L'Ecuyer
--    Efficient and portable combined random number generators
--    Comm ACM, 31(6), Jun 1988, pp742-749.
--
module Jack.Seed (
    Seed
  , mkSeed
  , randomSeed
  , nextMin
  , nextMax
  , nextSeed
  , nextInt53
  , splitSeed
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)

import Data.Int.Bits ((.&.))
import Data.Int53 (Int53)
import Data.Int53 as Int53
import Data.Tuple (Tuple(..))

import Prelude

-- | Splittable random number generator.
data Seed =
  Seed Int Int

-- | Create a new 'Seed' from a 32-bit integer.
mkSeed :: Int -> Seed
mkSeed s0 =
  let
    -- We want a non-negative number, but we can't just take the abs
    -- of s0 as -bottom == bottom.
    s =
      s0 .&. top

    -- The integer variables s1 and s2 must be initialized to values
    -- in the range [1, 2147483562] and [1, 2147483398] respectively. [1]
    s1 =
      s `mod` 2147483562

    q =
      s `div` 2147483562

    s2 =
      q `mod` 2147483398
  in
    Seed (s1 + 1) (s2 + 1)

-- | Create a random 'Seed' using the system random number generator.
randomSeed :: forall e. Eff ("random" :: RANDOM | e) Seed
randomSeed =
  mkSeed <$> randomInt bottom top

-- | The smallest possible value returned from 'next'.
nextMin :: Int
nextMin =
  1

-- | The largest possible value returned from 'next'.
nextMax :: Int
nextMax =
  2147483562

-- | Returns the next pseudo-random number in the sequence, and a new seed.
nextSeed :: Seed -> Tuple Int Seed
nextSeed (Seed s1 s2) =
  let
    k =
      s1 `div` 53668

    s1' =
      40014 * (s1 - k * 53668) - k * 12211

    s1'' =
      if s1' < 0 then
        s1' + 2147483563
      else
        s1'

    k' =
      s2 `div` 52774

    s2' =
      40692 * (s2 - k' * 52774) - k' * 3791

    s2'' =
      if s2' < 0 then
        s2' + 2147483399
      else
        s2'

    z =
      s1'' - s2''

    z' =
      if z < 1 then
        z + 2147483562
      else
        z
  in
    Tuple z' $ Seed (s1'') (s2'')

-- | Generate a random 'Int53' in the specified range.
-- | /Note this is not safe when (hi - lo) > 53-bits./
nextInt53 :: Int53 -> Int53 -> Seed -> Tuple Int53 Seed
nextInt53 lo hi seed =
  if lo > hi then
    nextInt53 hi lo seed
  else
    let
      --
      -- Probabilities of the most likely and least likely result will differ
      -- at most by a factor of (1 +- 1/q). Assuming Seed is uniform, of
      -- course.
      --
      -- On average, log q / log b more random values will be generated than
      -- the minimum.
      --
      b =
        Int53.fromInt nextMax - Int53.fromInt nextMin + one

      q =
        Int53.fromInt 1000

      k =
        hi - lo + one

      magtgt =
        k * q

      -- Generate random values until we exceed the target magnitude.
      loop mag v0 seed0 =
        if mag >= magtgt then
          Tuple v0 seed0
        else
          case nextSeed seed of
            Tuple x seed1 ->
              let
                v1 =
                  v0 * b + (Int53.fromInt x - Int53.fromInt nextMin)
              in
                loop (mag * b) v1 seed1
    in
      case loop one zero seed of
        Tuple v seedN ->
          Tuple (lo + (v `mod` k)) seedN

-- | Splits a random number generator in to two.
splitSeed :: Seed -> Tuple Seed Seed
splitSeed seed@(Seed s1 s2) =
  case nextSeed seed of
    Tuple _ (Seed t1 t2) ->
      let
        -- no statistical foundation for this!
        new_s1 =
          if s1 == 2147483562 then
            1
          else
            s1 + 1

        new_s2 =
          if s2 == 1 then
            2147483398
          else
            s2 - 1
      in
        Tuple
          (Seed new_s1 t2)
          (Seed t1 new_s2)
