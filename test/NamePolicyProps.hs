{-# LANGUAGE OverloadedStrings #-}

module NamePolicyProps (tests) where

import Hedgehog (Group(..), Property, property, forAll, assert, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Char as Char

import Agda.Compiler.Scala.NamePolicy (defaultNamePolicy, ctorName)
import Agda.Compiler.Scala.NameEnv (scalaKeywords)

tests :: Group
tests =
  Group
    "NamePolicy"
    [ ("Agda ctor -> valid Scala identifier", prop_ctorName_validScalaIdent)
    , ("is idempotent", prop_ctorName_idempotent)
    , ("[] ∷  -> Nil Cons etc", prop_ctorName_knownMappings)
    , ("scalaKeywords -> valid Scala identifier", prop_ctorName_notKeyword)
    ]

prop_ctorName_validScalaIdent :: Property
prop_ctorName_validScalaIdent = property $ do
  agdaCtor <- forAll (Gen.string (Range.linear 0 40) Gen.unicode)
  let scalaCtor = ctorName defaultNamePolicy agdaCtor
  assert (isValidScalaIdent scalaCtor)

prop_ctorName_idempotent :: Property
prop_ctorName_idempotent = property $ do
  agdaCtor <- forAll (Gen.string (Range.linear 0 40) Gen.unicode)
  let scalaCtor = ctorName defaultNamePolicy agdaCtor
  ctorName defaultNamePolicy scalaCtor === scalaCtor

prop_ctorName_knownMappings :: Property
prop_ctorName_knownMappings = property $ do
  ctorName defaultNamePolicy "[]" === "Nil"
  ctorName defaultNamePolicy "_∷_" === "Cons"
  ctorName defaultNamePolicy "_::_" === "Cons"

prop_ctorName_notKeyword :: Property
prop_ctorName_notKeyword = property $ do
  agdaCtor <- forAll (Gen.element scalaKeywords)
  let scalaCtor = ctorName defaultNamePolicy agdaCtor
  assert (scalaCtor `notElem` scalaKeywords)

isValidScalaIdent :: String -> Bool
isValidScalaIdent s =
  not (null s)
    && startsOk s
    && all okChar s
  where
    startsOk (c : _) = Char.isLetter c || c == '_'
    startsOk []      = False
    okChar c         = Char.isAlphaNum c || c == '_'
