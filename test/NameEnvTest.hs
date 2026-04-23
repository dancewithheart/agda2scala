module NameEnvTest (nameEnvTests) where

import Test.HUnit (Test(..), assertEqual, assertBool)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Agda.Compiler.Scala.NameEnv
  ( NameEnv(..)
  , emptyNameEnv
  , sanitizeScalaIdent
  , allocFreshLocal
  )

nameEnvTests :: Test
nameEnvTests = TestList
  [ TestLabel "emptyNameEnv" testEmpty
  , TestLabel "sanitize: empty -> x" testSanitizeEmpty
  , TestLabel "sanitize: illegal chars -> _" testSanitizeIllegal
  , TestLabel "sanitize: leading digit -> prefixed" testSanitizeLeadingDigit
  , TestLabel "sanitize: keyword -> suffix _" testSanitizeKeyword
  , TestLabel "fresh local: first alloc" testAllocFreshLocalFirst
  , TestLabel "fresh local: collision" testAllocFreshLocalCollision
  , TestLabel "fresh local: avoids keyword collision" testAllocFreshLocalKeywordCollision
  ]

testEmpty :: Test
testEmpty = TestCase $ do
  assertEqual "qname map empty" HM.empty (neQNameToScala emptyNameEnv)
  assertEqual "taken set empty" HS.empty (neTaken emptyNameEnv)
  assertEqual "counter 0" 0 (neCounter emptyNameEnv)

testSanitizeEmpty :: Test
testSanitizeEmpty = TestCase $
  assertEqual "sanitize empty" "x" (sanitizeScalaIdent "")

testSanitizeIllegal :: Test
testSanitizeIllegal = TestCase $
  assertEqual "sanitize illegal chars" "foo_bar_baz" (sanitizeScalaIdent "foo-bar.baz")

testSanitizeLeadingDigit :: Test
testSanitizeLeadingDigit = TestCase $
  assertEqual "sanitize leading digit" "x1abc" (sanitizeScalaIdent "1abc")

testSanitizeKeyword :: Test
testSanitizeKeyword = TestCase $
  assertEqual "sanitize keyword" "match_" (sanitizeScalaIdent "match")

testAllocFreshLocalFirst :: Test
testAllocFreshLocalFirst = TestCase $ do
  let (ne1, x) = allocFreshLocal emptyNameEnv "foo"
  assertEqual "first is base" "foo" x
  assertBool "foo taken" ("foo" `HS.member` neTaken ne1)

testAllocFreshLocalCollision :: Test
testAllocFreshLocalCollision = TestCase $ do
  let (ne1, x1) = allocFreshLocal emptyNameEnv "foo"
      (ne2, x2) = allocFreshLocal ne1 "foo"
      (ne3, x3) = allocFreshLocal ne2 "foo"
  assertEqual "x1" "foo" x1
  assertEqual "x2" "foo_1" x2
  assertEqual "x3" "foo_2" x3
  assertBool "all taken" (all (`HS.member` neTaken ne3) ["foo","foo_1","foo_2"])

testAllocFreshLocalKeywordCollision :: Test
testAllocFreshLocalKeywordCollision = TestCase $ do
  let (ne1, x1) = allocFreshLocal emptyNameEnv "match"
      (ne2, x2) = allocFreshLocal ne1 "match"
  -- sanitizeScalaIdent "match" => "match_"
  assertEqual "keyword gets underscore" "match_" x1
  -- next should become "match__1" (base is "match_", then freshen adds "_1")
  assertEqual "collision on keyword base" "match__1" x2
  assertBool "taken includes both" (all (`HS.member` neTaken ne2) ["match_","match__1"])
