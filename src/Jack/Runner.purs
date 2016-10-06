module Jack.Runner (
    jackMain
  , checkModule
  , checkModules
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)

import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Array as Array

import Jack.Property (Property, check)

import Prelude

jackMain :: forall e. Array String -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
jackMain modules = do
  ok <- checkModules modules
  unless ok $ exit 1

checkModule :: forall e. String -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
checkModule moduleName = do
  let
    pyjamas b1 name prop = do
      log $ "=== " <> name <> " from " <> moduleName <> " ==="
      b2 <- check prop
      log $ ""
      pure $ b1 && b2

  props <- findProperties moduleName
  StrMap.foldM pyjamas true props

checkModules :: forall e. Array String -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
checkModules modules =
  let
    loop b1 m =
      map (b1 && _) (checkModule m)
  in
    Array.foldM loop true modules

foreign import findProperties :: forall e. String -> Eff e (StrMap Property)

foreign import exit :: forall e. Int -> Eff e Unit
