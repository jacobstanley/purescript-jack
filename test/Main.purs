module Test.Main (
    main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Jack.Runner (jackMain)

import Prelude

main :: forall e. Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
main =
  jackMain [
      "Test.Foo"
    ]
