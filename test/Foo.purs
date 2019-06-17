module Test.Foo where

import Control.Lazy (fix)

import Data.Array as Array
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (fromCharArray, toCharArray)

import Jack.Combinators (boundedInt, chooseInt, elements, oneOfRec, arrayOf)
import Jack.Gen (Gen, reshrink)
import Jack.Property (Property, property, forAll)

import Prelude

data Exp =
    Lit Int
  | Var String
  | Lam String Exp
  | App Exp Exp

derive instance genericExp :: Generic Exp _

derive instance eqExp :: Eq Exp

instance showExp :: Show Exp where
  show x =
    genericShow x

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
    property $ x0 /= Var "never"

prop_salamander :: Property
prop_salamander =
  forAll (chooseInt 0 5) \x ->
  forAll (chooseInt 6 10) \y ->
    property $ x /= y

genAlphaNum :: Gen Char
genAlphaNum =
  elements $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genAlphaNumString :: Gen String
genAlphaNumString =
  map fromCharArray $ arrayOf genAlphaNum

prop_strings :: Property
prop_strings =
  forAll genAlphaNumString \xs ->
    property $ not $ contains (Pattern "#") xs

genEven :: Gen Int
genEven =
  map (\x -> (x / 2) * 2) $
  boundedInt

genEvenString :: Gen String
genEvenString =
  map show genEven

evens :: Array Char
evens =
  toCharArray "02468"

prop_even_strings_end_with_evens :: Property
prop_even_strings_end_with_evens =
  forAll genEvenString \str ->
    case Array.last $ toCharArray str of
      Nothing ->
        property false
      Just x ->
        property $ elem x evens
