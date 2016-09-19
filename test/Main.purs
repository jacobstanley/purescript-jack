module Test.Main where

import Control.Lazy (fix, defer)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Data.Generic (class Generic, gShow, gEq)

import Jack (Result, Gen)
import Jack (check, property, forAll)
import Jack (boundedInt, oneOfRec, reshrink, elements)

import Node.Process (PROCESS)
import Node.Process as Process

import Prelude

data Exp =
    Lit Int
  | Var String
  | Lam String Exp
  | App Exp Exp

derive instance genericExp :: Generic Exp

instance eqExp :: Eq Exp where
  eq =
    gEq

instance showExp :: Show Exp where
  show =
    gShow

genName :: Gen String
genName =
  elements ["x", "y", "z", "w"]

shrinkExp :: Exp -> Array Exp
shrinkExp xx =
  case xx of
    Lam _ x ->
      [x]
    App x y ->
      [x, y]
    _ ->
      []

genExp :: Gen Exp
genExp =
  fix $ \exp ->
    reshrink shrinkExp $
    oneOfRec [
        Lit <$> boundedInt
      , Var <$> genName
      ] [
        Lam <$> genName <*> exp
      , App <$> exp <*> exp
      ]

prop_example :: Gen Result
prop_example =
  forAll genExp \x0 ->
  forAll genExp \x1 ->
    property $ x0 == x1

main :: forall e. Eff ("random" :: RANDOM, "console" :: CONSOLE, "process" :: PROCESS | e) Unit
main =
  check prop_example >>= \ok ->
    if ok then
      Process.exit 0
    else
      Process.exit 1
