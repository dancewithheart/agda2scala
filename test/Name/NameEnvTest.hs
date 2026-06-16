module Name.NameEnvTest (tests) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Agda.Compiler.Scala.Name.NameEnv
    ( NameEnv (..)
    , allocFreshLocal
    , emptyNameEnv
    , sanitizeScalaIdent
    )

tests :: TestTree
tests =
    testGroup
        "Name.NameEnv / Scala identifier allocation"
        [ testCase "emptyNameEnv starts with no QName mappings, no taken names, and counter 0" test_emptyNameEnv
        , testCase "sanitizeScalaIdent maps an empty name to x" test_sanitize_empty
        , testCase "sanitizeScalaIdent replaces illegal Scala identifier characters with underscores" test_sanitize_illegalChars
        , testCase "sanitizeScalaIdent prefixes names that start with a digit" test_sanitize_leadingDigit
        , testCase "sanitizeScalaIdent avoids Scala keywords by adding a suffix" test_sanitize_keyword
        , testCase "allocFreshLocal uses the sanitized base name when it is free" test_allocFreshLocal_first
        , testCase "allocFreshLocal appends numeric suffixes on collisions" test_allocFreshLocal_collision
        , testCase "allocFreshLocal freshens names after keyword sanitization" test_allocFreshLocal_keywordCollision
        ]

test_emptyNameEnv :: IO ()
test_emptyNameEnv = do
    assertEqual "QName map" HM.empty (neQNameToScala emptyNameEnv)
    assertEqual "taken names" HS.empty (neTaken emptyNameEnv)
    assertEqual "counter" 0 (neCounter emptyNameEnv)

test_sanitize_empty :: IO ()
test_sanitize_empty =
    assertEqual "sanitized name" "x" (sanitizeScalaIdent "")

test_sanitize_illegalChars :: IO ()
test_sanitize_illegalChars =
    assertEqual "sanitized name" "foo_bar_baz" (sanitizeScalaIdent "foo-bar.baz")

test_sanitize_leadingDigit :: IO ()
test_sanitize_leadingDigit =
    assertEqual "sanitized name" "x1abc" (sanitizeScalaIdent "1abc")

test_sanitize_keyword :: IO ()
test_sanitize_keyword =
    assertEqual "sanitized name" "match_" (sanitizeScalaIdent "match")

test_allocFreshLocal_first :: IO ()
test_allocFreshLocal_first = do
    let (ne1, name) = allocFreshLocal emptyNameEnv "foo"
    assertEqual "allocated name" "foo" name
    assertBool "allocated name is marked taken" ("foo" `HS.member` neTaken ne1)

test_allocFreshLocal_collision :: IO ()
test_allocFreshLocal_collision = do
    let (ne1, name1) = allocFreshLocal emptyNameEnv "foo"
        (ne2, name2) = allocFreshLocal ne1 "foo"
        (ne3, name3) = allocFreshLocal ne2 "foo"

    assertEqual "first allocation" "foo" name1
    assertEqual "second allocation" "foo_1" name2
    assertEqual "third allocation" "foo_2" name3
    assertBool
        "all allocated names are marked taken"
        (all (`HS.member` neTaken ne3) ["foo", "foo_1", "foo_2"])

test_allocFreshLocal_keywordCollision :: IO ()
test_allocFreshLocal_keywordCollision = do
    let (ne1, name1) = allocFreshLocal emptyNameEnv "match"
        (ne2, name2) = allocFreshLocal ne1 "match"

    assertEqual "keyword is sanitized first" "match_" name1
    assertEqual "fresh suffix is added to sanitized keyword base" "match__1" name2
    assertBool
        "both allocated names are marked taken"
        (all (`HS.member` neTaken ne2) ["match_", "match__1"])
