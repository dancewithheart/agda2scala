{-# LANGUAGE OverloadedStrings #-}

module NamePolicyProps (tests) where

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Hedgehog (Gen, Group (..), Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.NameEnv (scalaKeywords)
import Agda.Compiler.Scala.NamePolicy (NamePolicy (..), ctorName, defaultNamePolicy)

tests :: [Group]
tests =
    [ namingGroup
    , mergeGroup
    ]

namingGroup :: Group
namingGroup =
    Group
        "NamePolicy / naming"
        [ ("Agda ctor -> valid Scala identifier", prop_ctorName_validScalaIdent)
        , ("is idempotent", prop_ctorName_idempotent)
        , ("[] ∷  -> Nil Cons etc", prop_ctorName_knownMappings)
        , ("scalaKeywords -> valid Scala identifier", prop_ctorName_notKeyword)
        ]

mergeGroup :: Group
mergeGroup =
    Group
        "NamePolicy / merge"
        [ ("is right biased", prop_namePolicy_overrides_are_right_biased)
        , ("Semigroup: associative", prop_semigroup_assoc)
        , ("Monoid: left identity", prop_monoid_left_identity)
        , ("Monoid: right identity", prop_monoid_right_identity)
        ]

genPolicy :: Gen NamePolicy
genPolicy = do
    kvs <- Gen.list (Range.linear 0 20) genKV
    pure mempty{npCtorMap = HM.fromList kvs}
  where
    genKV = do
        k <- Gen.string (Range.linear 0 12) Gen.alphaNum
        v <- Gen.string (Range.linear 0 12) Gen.alphaNum
        pure (k, v)

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

-- TODO consider more eneral property
-- something like forall policyL (policyR + mappingM) then mappingM exists in merged result
-- (maybe sth like optics laws put put == put)
prop_namePolicy_overrides_are_right_biased :: Property
prop_namePolicy_overrides_are_right_biased = property $ do
    let userOverrides =
            mempty{npCtorMap = HM.fromList [("[]", "EmptyList")]}
        pol = defaultNamePolicy <> userOverrides
    ctorName pol "[]" === "EmptyList"

isValidScalaIdent :: String -> Bool
isValidScalaIdent s =
    not (null s)
        && startsOk s
        && all okChar s
  where
    startsOk (c : _) = Char.isLetter c || c == '_'
    startsOk [] = False
    okChar c = Char.isAlphaNum c || c == '_'

prop_semigroup_assoc :: Property
prop_semigroup_assoc = property $ do
    a <- forAll genPolicy
    b <- forAll genPolicy
    c <- forAll genPolicy
    (a <> b) <> c === a <> (b <> c)

prop_monoid_left_identity :: Property
prop_monoid_left_identity = property $ do
    a <- forAll genPolicy
    mempty <> a === a

prop_monoid_right_identity :: Property
prop_monoid_right_identity = property $ do
    a <- forAll genPolicy
    a <> mempty === a
