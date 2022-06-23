module Test.Main
  ( main
  ) where

import Effect (Effect)

import Jack.Runner (jackMain)

import Prelude

main :: Effect Unit
main =
  jackMain
    [ "Test.Foo"
    ]
