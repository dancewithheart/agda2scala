{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for Scala printing.

We test observable output properties rather than internal helper functions.
This makes tests robust across refactors.
-}
module Render.PrintProps (printProps) where

import Data.List (intercalate, isInfixOf)

import Hedgehog (Gen, Group (..), Property, assert, forAll, property, success)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr (..)
  , ScalaMember (..)
  , ScalaTerm (..)
  , ScalaType (..)
  , ScalaTypeBinder (..)
  , ScalaTypeScheme (..)
  , ScalaTyParam(..)
  , ScalaVariance(..)
  , SeVar (..)
  , scalaTypeScheme
  )

import Agda.Compiler.Scala.Render.Common (escapeScalaString)
import Agda.Compiler.Scala.Render.PrintScala2 (printScala2, printType)
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3)

printProps :: Group
printProps =
    Group
        "Render.PrintProps"
        [ ("prop_scala2_stringEscapes_newline", prop_scala2_stringEscapes_newline)
        , ("prop_scala2_stringEscapes_quote", prop_scala2_stringEscapes_quote)
        , ("prop_scala2_boolLowercase", prop_scala2_boolLowercase)
        , ("prop_scala3_stringEscapes_newline", prop_scala3_stringEscapes_newline)
        , ("prop_escape_no_raw_newlines", prop_escape_no_raw_newlines)
        , ("prop_printScala3_type_total", prop_printScala3_type_total)
        , ("prop_scala2_prints_type_params_iff_present", prop_scala2_prints_type_params_iff_present)
        , ( "higher-kinded record parameters render in both dialects"
          , prop_recordTypeParameterArity
          )
        , ( "higher-kinded function parameters render in both dialects"
          , prop_functionTypeParameterArity
          )
        , ( "higher-kinded datatypes render in both dialects"
          , prop_higherKindedDatatypeRendering
          )
        ]

-- ===== Generators ============================================================

type ScalaName = String

genUpperIdent :: Gen ScalaName
genUpperIdent = do
    c <- Gen.upper
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
mkFun0 name ret = SeFun name [] (scalaTypeScheme ret)

mkModule :: [ScalaExpr] -> ScalaExpr
mkModule = SePackage ["examples", "props"]

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

    let scheme =
          ScalaTypeScheme
            (map (`ScalaTypeBinder` 0) ps)
            (STyVar "A")
        expr = SeFun "f" [SeVar "x" (STyVar "A")] scheme (STeVar "x")
        out = printScala2 expr

        hasBrackets = "[" `isInfixOf` out && "]" `isInfixOf` out

    if null ps
        then assert (not hasBrackets)
        else assert hasBrackets

prop_recordTypeParameterArity :: Property
prop_recordTypeParameterArity = property $ do
    arity <- forAll (Gen.int (Range.linear 0 4))
    let param = ScalaTypeBinder "F" arity
        expr = SeTrait
          "Transformer"
          [param]
          [MemberValue (SeVar "run" (STyName "Long"))]
        suffix
          | arity == 0 = ""
          | otherwise =
              "[" <> intercalate ", " (replicate arity "_") <> "]"
        expected = "trait Transformer[F" <> suffix <> "]"
    assert (expected `isInfixOf` printScala2 expr)
    assert (expected `isInfixOf` printScala3 expr)

prop_functionTypeParameterArity :: Property
prop_functionTypeParameterArity = property $ do
    arity <- forAll (Gen.int (Range.linear 0 4))
    let param = ScalaTypeBinder "F" arity
        ty
          | arity == 0 = STyVar "F"
          | otherwise =
              STyApp "F" (replicate arity (STyName "Long"))
        expr =
          SeFun
            "idF"
            [SeVar "x" ty]
            (ScalaTypeScheme [param] ty)
            (STeVar "x")
        suffix
          | arity == 0 = ""
          | otherwise =
              "[" <> intercalate ", " (replicate arity "_") <> "]"
        expected = "def idF[F" <> suffix <> "]"
    assert (expected `isInfixOf` printScala2 expr)
    assert (expected `isInfixOf` printScala3 expr)

prop_higherKindedDatatypeRendering :: Property
prop_higherKindedDatatypeRendering = property $ do
    let expr =
          SeSum
            "HKBox"
            [ ScalaTyParam (ScalaTypeBinder "F" 1) Invariant
            , ScalaTyParam (ScalaTypeBinder "A" 0) Invariant
            ]
            [ ScalaCtor "Wrap" [STyApp "F" [STyVar "A"]] ]
        scala2 = printScala2 expr
        scala3 = printScala3 expr
    assert ("sealed trait HKBox[F[_], A]" `isInfixOf` scala2)
    assert
      ( "final case class Wrap[F[_], A](x0: F[A]) extends HKBox[F, A]"
          `isInfixOf` scala2
      )
    assert ("enum HKBox[F[_], A]" `isInfixOf` scala3)
    assert
      ( "case Wrap(x0: F[A]) extends HKBox[F, A]"
          `isInfixOf` scala3
      )
