module Jack.Runner (
    jackMain
  , checkModule
  , checkModules
  ) where

import Data.Traversable (for)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)

import Data.Tuple.Nested (type (/\), (/\))
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
    rat ns = do
      log ""
      log "*** Warning ***"
      _ <- for ns $ \n -> do
        log $ "- " <> n <> " starts with prop_ but is not of type Property"
      log ""
      log "  You may be trying to define something like:"
      log "    prop_example :: Int -> Property"
      log ""
      log "  Jack doesn't have Arbitrary like QuickCheck, and so cannot automatically provide"
      log "  property inputs in the same way; it uses forAll instead."
      log "  See https://github.com/jystic/purescript-jack#getting-started for some property"
      log "  examples to get started with."
      log ""
    pyjamas b1 name prop = do
      log $ "=== " <> name <> " from " <> moduleName <> " ==="
      b2 <- check prop
      log $ ""
      pure $ b1 && b2

  (badPropNames /\ props) <- findProperties moduleName (/\)

  when (not <<< Array.null $ badPropNames) $ rat badPropNames
  StrMap.foldM pyjamas true props

checkModules :: forall e. Array String -> Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Boolean
checkModules modules =
  let
    loop b1 m =
      map (b1 && _) (checkModule m)
  in
    Array.foldM loop true modules

foreign import findProperties ::
  forall e a b.
  String ->
  (a -> b -> a /\ b) ->
  Eff e (Array String /\ StrMap Property)

foreign import exit :: forall e. Int -> Eff e Unit
