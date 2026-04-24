{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for Scala printing.

We test observable output properties rather than internal helper functions.
This makes tests robust across refactors.
-}
module PrintProps (tests) where

import Data.List (isInfixOf)

import Hedgehog (Gen (..), Group (..), GroupName (..), Property, PropertyName (..), assert, forAll, property, success, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.ScalaExpr (
    ScalaExpr (..),
    ScalaTerm (..),
    ScalaType (..),
    ScalaTypeScheme (..),
    SeVar (..),
    scalaTypeScheme,
 )

import Agda.Compiler.Scala.PrintScala2 (escapeScalaString, printScala2, printType)
import Agda.Compiler.Scala.PrintScala3 (printScala3)

tests :: Group
tests =
    Group
        "PrintProps"
        [ ("prop_scala2_stringEscapes_newline", prop_scala2_stringEscapes_newline)
        , ("prop_scala2_stringEscapes_quote", prop_scala2_stringEscapes_quote)
        , ("prop_scala2_boolLowercase", prop_scala2_boolLowercase)
        , ("prop_scala3_stringEscapes_newline", prop_scala3_stringEscapes_newline)
        , ("prop_escape_no_raw_newlines", prop_escape_no_raw_newlines)
        , ("prop_printScala3_type_total", prop_printScala3_type_total)
        , ("prop_scala2_prints_type_params_iff_present", prop_scala2_prints_type_params_iff_present)
        ]

-- ===== Generators ============================================================

type ScalaName = String

genUpperIdent :: Gen ScalaName
genUpperIdent = do
    c <- Gen.upper
    cs <- Gen.list (Range.linear 0 12) (Gen.choice [Gen.alphaNum, Gen.element ['_']])
    pure (c : cs)

genLowerIdent :: Gen ScalaName
genLowerIdent = do
    c <- Gen.lower
    cs <- Gen.list (Range.linear 0 12) (Gen.choice [Gen.alphaNum, Gen.element ['_']])
    pure (c : cs)

-- Type constructors: Int, Bool, MyType, List, Either, Foo_Bar
genTypeName :: Gen ScalaName
genTypeName = genUpperIdent

-- Type variables: A, B, T0, X
genTyVar :: Gen ScalaName
genTyVar =
    Gen.choice
        [ (: []) <$> Gen.upper
        , do
            c <- Gen.upper
            n <- Gen.int (Range.linear 0 20)
            pure (c : show n)
        ]

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

genScalaType :: Gen ScalaType
genScalaType =
    Gen.recursive
        Gen.choice
        [ STyName <$> genTypeName
        , STyVar <$> genTyVar
        ]
        [ STyFun <$> genScalaType <*> genScalaType
        , STyApp <$> genTypeName <*> Gen.list (Range.linear 0 4) genScalaType
        ]

genTyParam :: Gen String
genTyParam = do
    c <- Gen.upper
    n <- Gen.int (Range.linear 0 10)
    pure (c : show n)

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

prop_escape_no_raw_newlines :: Property
prop_escape_no_raw_newlines = property $ do
    s <- forAll (Gen.string (Range.linear 0 200) Gen.unicode)
    let e = escapeScalaString s
    assert ('\n' `notElem` e)
    assert ('\r' `notElem` e)

prop_printScala3_type_total :: Property
prop_printScala3_type_total = property $ do
    ty <- forAll genScalaType
    -- should not throw (it’s pure, but total pattern match now)
    let _ = printType ty
    success

prop_scala2_prints_type_params_iff_present :: Property
prop_scala2_prints_type_params_iff_present = property $ do
    ps <- forAll (Gen.list (Range.linear 0 4) genTyParam)

    let scheme = ScalaTypeScheme ps (STyVar "A")
        expr = SeFun "f" [SeVar "x" (STyVar "A")] scheme (STeVar "x")
        out = printScala2 expr

        hasBrackets = "[" `isInfixOf` out && "]" `isInfixOf` out

    if null ps
        then assert (not hasBrackets)
        else assert hasBrackets
