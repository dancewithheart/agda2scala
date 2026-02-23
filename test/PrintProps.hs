{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for Scala printing.
--
-- We test observable output properties rather than internal helper functions.
-- This makes tests robust across refactors.
module PrintProps (tests) where

import Data.List (isInfixOf)

import Hedgehog (Group(..), Gen(..), Property, PropertyName(..), GroupName(..), property, forAll, assert, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.ScalaExpr
  ( ScalaExpr(..)
  , ScalaTerm(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , SeVar(..)
  , scalaTypeScheme
  )

import Agda.Compiler.Scala.PrintScala2 (printScala2)
import Agda.Compiler.Scala.PrintScala3 (printScala3)

tests :: Group
tests =
  Group
    "PrintProps"
    [ ("prop_scala2_stringEscapes_newline", prop_scala2_stringEscapes_newline)
    , ("prop_scala2_stringEscapes_quote", prop_scala2_stringEscapes_quote)
    , ("prop_scala2_boolLowercase", prop_scala2_boolLowercase)
    , ("prop_scala3_stringEscapes_newline", prop_scala3_stringEscapes_newline)
    ]

-- ===== Generators ============================================================

genPlainChunk :: Gen String
genPlainChunk = Gen.string (Range.linear 0 20) Gen.alphaNum

genWithNewline :: Gen String
genWithNewline = do
  a <- genPlainChunk
  b <- genPlainChunk
  pure (a <> "\n" <> b)

genWithQuote :: Gen String
genWithQuote = do
  a <- genPlainChunk
  b <- genPlainChunk
  pure (a <> "\"" <> b)

-- ===== Small AST builders ====================================================

mkFun0 :: String -> ScalaType -> ScalaTerm -> ScalaExpr
mkFun0 name ret body =
  SeFun name [] (scalaTypeScheme ret) body

mkModule :: [ScalaExpr] -> ScalaExpr
mkModule defs = SePackage ["examples", "props"] defs

render2 :: ScalaExpr -> String
render2 = printScala2

render3 :: ScalaExpr -> String
render3 = printScala3

-- ===== Output checks =========================================================

contains :: String -> String -> Bool
contains = isInfixOf

-- ===== Properties ============================================================

-- Scala2: if input contains '\n' in a string literal, output should contain "\\n"
-- and should not contain an actual newline inside the literal.
prop_scala2_stringEscapes_newline :: Property
prop_scala2_stringEscapes_newline = property $ do
  s <- forAll genWithNewline
  let out = render2 (mkModule [mkFun0 "f" (STyName "String") (STeLitString s)])
  assert (contains "\\n" out)

-- Scala2: if input contains '"', output should contain "\\\""
prop_scala2_stringEscapes_quote :: Property
prop_scala2_stringEscapes_quote = property $ do
  s <- forAll genWithQuote
  let out = render2 (mkModule [mkFun0 "f" (STyName "String") (STeLitString s)])
  assert (contains "\\\"" out)

-- Scala2: boolean literals must be "true"/"false" (Scala lowercase)
prop_scala2_boolLowercase :: Property
prop_scala2_boolLowercase = property $ do
  b <- forAll Gen.bool
  let out = render2 (mkModule [mkFun0 "f" (STyName "Boolean") (STeLitBool b)])
  if b
    then assert (contains "true" out && not (contains "True" out))
    else assert (contains "false" out && not (contains "False" out))

-- Scala3: same string escaping property (you fixed PrintScala3 similarly)
prop_scala3_stringEscapes_newline :: Property
prop_scala3_stringEscapes_newline = property $ do
  s <- forAll genWithNewline
  let out = render3 (mkModule [mkFun0 "f" (STyName "String") (STeLitString s)])
  assert (contains "\\n" out)
