module PrintScala2Test ( printScala2Tests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScala2
  ( printScala2
  , printSealedTrait
  , printCaseObject
  , printPackageAndObject
  , combineLines
  , printCaseClass
  )
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaExpr(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , ScalaTerm(..)
  , ScalaCtor(..)
  , ScalaType(..)
  , SeVar(..)
  )

testPrintCaseObject :: Test
testPrintCaseObject = TestCase $
  assertEqual "printCaseObject"
    "case object Light extends Color"
    (printCaseObject "Color" "Light")

testPrintSealedTrait :: Test
testPrintSealedTrait = TestCase $
  assertEqual "printSealedTrait"
    "sealed trait Color"
    (printSealedTrait "Color")

testObjectWhenNoPackage :: Test
testObjectWhenNoPackage = TestCase $
  assertEqual "printPackageAndObject"
    "object adts"
    (printPackageAndObject ["adts"])

testPrintPackageAndObject :: Test
testPrintPackageAndObject = TestCase $
  assertEqual "printPackageAndObject"
    "package example\n\nobject adts"
    (printPackageAndObject ["example", "adts"])

testPrintMultiplePartPackageAndObject :: Test
testPrintMultiplePartPackageAndObject = TestCase $
  assertEqual "printPackageAndObject"
    "package org.example\n\nobject adts"
    (printPackageAndObject ["org", "example", "adts"])

testCombineLines :: Test
testCombineLines = TestCase $
  assertEqual "combineLines"
    "a\nb"
    (combineLines ["", "a", "", "", "b", "", "", ""])

testPrintCaseClass :: Test
testPrintCaseClass = TestCase $
  assertEqual "printCaseClass"
    "final case class RgbPair(snd: Bool, fst: Rgb)"
    (printCaseClass "RgbPair"
      [ SeVar "snd" (STyName "Bool")
      , SeVar "fst" (STyName "Rgb")
      ])

testPrintScala2 :: Test
testPrintScala2 = TestCase $
  assertEqual "printScala2"
    expected
    (printScala2 $ SePackage ["adts"] moduleContent)
  where
    rgbAdt =
      SeSum "Rgb"
        [ ScalaCtor "Red"   []
        , ScalaCtor "Green" []
        , ScalaCtor "Blue"  []
        ]
    colorAdt =
      SeSum "Color"
        [ ScalaCtor "Light" []
        , ScalaCtor "Dark"  []
        ]
    blank = SeUnhandled "" ""
    moduleContent = [rgbAdt, blank, blank, blank, colorAdt, blank, blank]
    expected =
      "object adts\n" <>
      "{\n" <>
      "\n" <>
      "sealed trait Rgb\n" <>
      "object Rgb {\n" <>
      "  case object Red extends Rgb\n" <>
      "  case object Green extends Rgb\n" <>
      "  case object Blue extends Rgb\n" <>
      "\n" <>          -- this blank line is currently produced by your printer
      "}\n" <>
      "\n" <>
      "sealed trait Color\n" <>
      "object Color {\n" <>
      "  case object Light extends Color\n" <>
      "  case object Dark extends Color\n" <>
      "\n" <>          -- likewise
      "}\n" <>
      "}\n"

testPolyDef :: Test
testPolyDef = TestCase $
  assertEqual "prints def with [A] and return A"
    expected
    (printScala2 expr)
  where
    expr =
      SeFun
        "id"
        [SeVar "x1" (STyVar "A")]
        (ScalaTypeScheme ["A"] (STyVar "A"))
        (STeVar "x1")

    expected =
      "def id[A](x1: A): A = x1\n"

printScala2Tests :: Test
printScala2Tests = TestList
  [ TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printObject" testObjectWhenNoPackage
  , TestLabel "printPackageAndObject" testPrintPackageAndObject
  , TestLabel "printPackageAndObject 2" testPrintMultiplePartPackageAndObject
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printCaseClass" testPrintCaseClass
  , TestLabel "printScala2" testPrintScala2
  , TestLabel "printScala2 polymorphic def" testPolyDef
  ]
