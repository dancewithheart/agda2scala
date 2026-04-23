{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for NameEnv.
--
-- Name hygiene is one of the easiest areas to make property-testable.
--
-- Scala keywords:
--   Scala 3 keywords list: https://docs.scala-lang.org/scala3/reference/changed-features/keywords.html
module NameEnvProps (tests) where

import Data.Char (isAlphaNum, isLetter)
import Data.List (mapAccumL, nub)
import qualified Data.Set as Set

import Hedgehog (Group(..), Gen, Property, property, forAll, assert, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.NameEnv
  ( emptyNameEnv
  , sanitizeScalaIdent
  , allocFreshLocal
  )

tests :: Group
tests =
  Group
    "NameEnvProps"
    [ ("prop_sanitize_nonEmpty", prop_sanitize_nonEmpty)
    , ("prop_sanitize_validScalaIdentChars", prop_sanitize_validScalaIdentChars)
    , ("prop_sanitize_notKeyword", prop_sanitize_notKeyword)
    , ("prop_allocFreshLocal_unique", prop_allocFreshLocal_unique)
    , ("prop_allocFreshLocal_deterministic", prop_allocFreshLocal_deterministic)
    , ("prop_sanitize_idempotent", prop_sanitize_idempotent)
    , ("prop_allocFreshLocal_unique_nonKeyword", prop_allocFreshLocal_unique_nonKeyword)
    ]

-- ===== Generators ============================================================

genAnyString :: Gen String
genAnyString =
  Gen.frequency
    [ (6, Gen.string (Range.linear 0 30) Gen.alphaNum)
    , (2, Gen.string (Range.linear 0 30) Gen.unicode)
    , (2, Gen.string (Range.linear 0 30) (Gen.element ("_-.$#@! \n\t\"\\\\" :: String)))
    ]

genRawName :: Gen String
genRawName =
  Gen.string (Range.linear 0 40) genNameChar
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
    [ "type","val","var","def","class","object","trait","enum","given","using"
    , "match","case","if","then","else","for","yield","do","while","try","catch","finally"
    , "new","this","extends","with","import","package","private","override"
    , "true","false","null"
    ]

isValidScalaIdent :: String -> Bool
isValidScalaIdent s =
  not (null s)
    && startsOk s
    && all okChar s
  where
    startsOk (c : _) = isLetter c || c == '_'
    startsOk []      = False

    okChar c = isAlphaNum c || c == '_'

isScalaKeyword :: String -> Bool
isScalaKeyword s = Set.member s scalaKeywords

-- ===== Properties ============================================================

prop_sanitize_nonEmpty :: Property
prop_sanitize_nonEmpty = property $ do
  raw <- forAll genRawName
  let s = sanitizeScalaIdent raw
  assert (not (null s))

prop_sanitize_validScalaIdentChars :: Property
prop_sanitize_validScalaIdentChars = property $ do
  raw <- forAll genRawName
  let s = sanitizeScalaIdent raw
  assert (isValidScalaIdent s)

prop_sanitize_notKeyword :: Property
prop_sanitize_notKeyword = property $ do
  raw <- forAll genRawName
  let s = sanitizeScalaIdent raw
  -- your sanitize adds "_" if keyword, so we should never land on a keyword
  assert (Set.notMember s scalaKeywords)

prop_allocFreshLocal_unique :: Property
prop_allocFreshLocal_unique = property $ do
  raws <- forAll genManyRawNames
  let names = allocMany raws
  length names === length (nub names)
  where
    allocMany :: [String] -> [String]
    allocMany = go emptyNameEnv []
      where
        go _ acc [] = reverse acc
        go ne acc (r : rs) =
          let (ne', n) = allocFreshLocal ne r
          in go ne' (n : acc) rs

prop_allocFreshLocal_deterministic :: Property
prop_allocFreshLocal_deterministic = property $ do
  raws <- forAll genManyRawNames
  let out1 = allocMany raws
      out2 = allocMany raws
  out1 === out2
  where
    allocMany :: [String] -> [String]
    allocMany = go emptyNameEnv []
      where
        go _ acc [] = reverse acc
        go ne acc (r : rs) =
          let (ne', n) = allocFreshLocal ne r
          in go ne' (n : acc) rs

prop_sanitize_idempotent :: Property
prop_sanitize_idempotent = property $ do
  s <- forAll genAnyString
  sanitizeScalaIdent (sanitizeScalaIdent s) === sanitizeScalaIdent s

-- allocFreshLocal never returns a keyword, never duplicates
prop_allocFreshLocal_unique_nonKeyword :: Property
prop_allocFreshLocal_unique_nonKeyword = property $ do
  bases <- forAll (Gen.list (Range.linear 0 200) genAnyString)

  let (_, outs) =
        mapAccumL
          (\ne b -> let (ne', n) = allocFreshLocal ne b in (ne', n))
          emptyNameEnv
          bases

  assert (length outs == length (nub outs))
  assert (all (not . isScalaKeyword) outs)
