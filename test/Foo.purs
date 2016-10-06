module Test.Foo where

import Control.Lazy (fix)

import Data.Generic (class Generic, gShow, gEq)
import Data.String (toCharArray, fromCharArray, contains)

import Jack.Combinators (boundedInt, chooseInt, elements, oneOfRec, arrayOf)
import Jack.Gen (Gen, reshrink)
import Jack.Property (Property, property, forAll)

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

prop_example :: Property
prop_example =
  forAll genExp \x0 ->
  forAll genExp \x1 ->
    property $ x0 == x1

prop_foo :: Property
prop_foo =
  forAll (chooseInt 0 5) \x ->
  forAll (chooseInt 0 5) \y ->
    property true

prop_salamander :: Property
prop_salamander =
  forAll (chooseInt 0 5) \x ->
  forAll (chooseInt 0 5) \y ->
    property $ x == y

genAlphaNum :: Gen Char
genAlphaNum =
  elements $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genAlphaNumString :: Gen String
genAlphaNumString =
  map fromCharArray $ arrayOf genAlphaNum

prop_strings :: Property
prop_strings =
  forAll genAlphaNumString \xs ->
    property $ not $ contains "x" xs
