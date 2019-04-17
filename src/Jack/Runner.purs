module Jack.Runner (
    jackMain
  , checkModule
  , checkModules
  ) where

import Data.Array as Array

import Effect (Effect)
import Effect.Console (log)

import Foreign.Object (Object)
import Foreign.Object as Object

import Jack.Property (Property, check)

import Prelude

jackMain :: Array String -> Effect Unit
jackMain modules = do
  ok <- checkModules modules
  unless ok $ exit 1

checkModule :: String -> Effect Boolean
checkModule moduleName = do
  let
    pyjamas b1 name prop = do
      log $ "=== " <> name <> " from " <> moduleName <> " ==="
      b2 <- check prop
      log $ ""
      pure $ b1 && b2

  props <- findProperties moduleName
  Object.foldM pyjamas true props

checkModules :: Array String -> Effect Boolean
checkModules modules =
  let
    loop b1 m =
      map (b1 && _) (checkModule m)
  in
    Array.foldM loop true modules

foreign import findProperties :: String -> Effect (Object Property)

foreign import exit :: Int -> Effect Unit
