{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for NameEnv.

Name hygiene is one of the easiest areas to make property-testable.

Scala keywords:
  Scala 3 keywords list: https://docs.scala-lang.org/scala3/reference/changed-features/keywords.html
-}
module Name.NameEnvProps (nameEnvProps) where

import Data.Char (isAlphaNum, isLetter)
import Data.List (mapAccumL, nub)
import qualified Data.HashSet as HS
import qualified Data.Set as Set

import Hedgehog (Gen, Group (..), Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.Name.NameEnv
    ( NameEnv (..)
    , allocFreshLocal
    , emptyNameEnv
    , freshNumberedNamesAvoiding
    , sanitizeScalaIdent
    )

nameEnvProps :: Group
nameEnvProps =
    Group
        "Name.NameEnv / Scala identifier hygiene"
        [ ("sanitized identifiers are never empty", prop_sanitize_nonEmpty)
        , ("sanitized identifiers contain only Scala identifier characters", prop_sanitize_validScalaIdentChars)
        , ("sanitized identifiers are never Scala keywords", prop_sanitize_notKeyword)
        , ("sanitizing an already sanitized identifier is stable (idempotent)", prop_sanitize_idempotent)
        , ("fresh local allocation returns unique names", prop_allocFreshLocal_unique)
        , ("fresh local allocation is deterministic for the same inputs", prop_allocFreshLocal_deterministic)
        , ("fresh local allocation never returns a Scala keyword", prop_allocFreshLocal_unique_nonKeyword)
        , ("fresh local allocation marks every returned name as taken", prop_allocFreshLocal_marksAllocatedNamesTaken)
        , ("first allocation returns the sanitized base name when it is available", prop_allocFreshLocal_firstUsesSanitizedBase)
        , ("numbered fresh names have the requested count and avoid reserved names", prop_freshNumberedNames_areFresh)
        ]

-- ===== Generators ============================================================

genAnyString :: Gen String
genAnyString =
    Gen.frequency
        [ (6, Gen.string (Range.linear 0 30) Gen.alphaNum)
        , (2, Gen.string (Range.linear 0 30) Gen.unicode)
        , (2, Gen.string (Range.linear 0 30) (Gen.element ("_-.$#@! \n\t\"\\" :: String)))
        ]

genRawName :: Gen String
genRawName = Gen.string (Range.linear 0 40) genNameChar
  where
    -- include some "annoying" characters to stress sanitization
    genNameChar =
        Gen.frequency
            [ (8, Gen.alphaNum)
            , (2, Gen.element [' ', '-', '.', '/', '\\', '\n', '\t', ':', ';', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '+', '='])
            ]

genNonEmptyRawName :: Gen String
genNonEmptyRawName = Gen.string (Range.linear 1 40) genNameChar
  where
    genNameChar = Gen.frequency [(8, Gen.alphaNum), (2, Gen.element [' ', '-', '.', '/', '\\', '\n', '\t'])]

genManyRawNames :: Gen [String]
genManyRawNames = Gen.list (Range.linear 1 200) genNonEmptyRawName

-- ===== Helpers ===============================================================

scalaKeywords :: Set.Set String
scalaKeywords =
    Set.fromList
        [ "type"
        , "val"
        , "var"
        , "def"
        , "class"
        , "object"
        , "trait"
        , "enum"
        , "given"
        , "using"
        , "match"
        , "case"
        , "if"
        , "then"
        , "else"
        , "for"
        , "yield"
        , "do"
        , "while"
        , "try"
        , "catch"
        , "finally"
        , "new"
        , "this"
        , "extends"
        , "with"
        , "import"
        , "package"
        , "private"
        , "override"
        , "true"
        , "false"
        , "null"
        ]

isValidScalaIdent :: String -> Bool
isValidScalaIdent s =
    not (null s)
      && startsOk s
      && all okChar s
  where
    startsOk (c : _) = isLetter c || c == '_'
    startsOk [] = False
    okChar c = isAlphaNum c || c == '_'

isScalaKeyword :: String -> Bool
isScalaKeyword s = Set.member s scalaKeywords

allocMany :: [String] -> (NameEnv, [String])
allocMany = mapAccumL step emptyNameEnv
  where
    step ne raw = let (ne', name) = allocFreshLocal ne raw in (ne', name)

-- ===== Properties ============================================================

prop_sanitize_nonEmpty :: Property
prop_sanitize_nonEmpty = property $ do
    raw <- forAll genRawName
    let name = sanitizeScalaIdent raw
    assert (not (null name))

prop_sanitize_validScalaIdentChars :: Property
prop_sanitize_validScalaIdentChars = property $ do
    raw <- forAll genRawName
    let name = sanitizeScalaIdent raw
    assert (isValidScalaIdent name)

prop_sanitize_notKeyword :: Property
prop_sanitize_notKeyword = property $ do
    raw <- forAll genRawName
    let name = sanitizeScalaIdent raw
    -- your sanitize adds "_" if keyword, so we should never land on a keyword
    assert (Set.notMember name scalaKeywords)

prop_sanitize_idempotent :: Property
prop_sanitize_idempotent = property $ do
    raw <- forAll genAnyString
    sanitizeScalaIdent (sanitizeScalaIdent raw) === sanitizeScalaIdent raw

prop_allocFreshLocal_unique :: Property
prop_allocFreshLocal_unique = property $ do
    rawNames <- forAll genManyRawNames
    let (_, names) = allocMany rawNames
    length names === length (nub names)

prop_allocFreshLocal_deterministic :: Property
prop_allocFreshLocal_deterministic = property $ do
    rawNames <- forAll genManyRawNames
    let (_, names1) = allocMany rawNames
        (_, names2) = allocMany rawNames
    names1 === names2

{- HLINT ignore "Hoist not" -}
-- allocFreshLocal never returns a keyword, never duplicates
prop_allocFreshLocal_unique_nonKeyword :: Property
prop_allocFreshLocal_unique_nonKeyword = property $ do
    rawNames <- forAll (Gen.list (Range.linear 0 200) genAnyString)
    let (_, names) = allocMany rawNames
    assert (length names == length (nub names))
    assert (all (not . isScalaKeyword) names)

prop_allocFreshLocal_marksAllocatedNamesTaken :: Property
prop_allocFreshLocal_marksAllocatedNamesTaken = property $ do
    rawNames <- forAll genManyRawNames
    let (nameEnv, names) = allocMany rawNames
    assert (all (`HS.member` neTaken nameEnv) names)

prop_allocFreshLocal_firstUsesSanitizedBase :: Property
prop_allocFreshLocal_firstUsesSanitizedBase = property $ do
    rawName <- forAll genRawName
    let (_nameEnv, allocatedName) = allocFreshLocal emptyNameEnv rawName
    allocatedName === sanitizeScalaIdent rawName

prop_freshNumberedNames_areFresh :: Property
prop_freshNumberedNames_areFresh = property $ do
  count <- forAll (Gen.int (Range.linear 0 50))
  takenIndices <- forAll (Gen.list (Range.linear 0 80) (Gen.int (Range.linear 0 100)))
  let taken = HS.fromList [ "p" <> show i | i <- takenIndices ]
      generated = freshNumberedNamesAvoiding taken "p" count
  length generated === count
  length (nub generated) === count
  assert (all (\name -> not (name `HS.member` taken)) generated)
