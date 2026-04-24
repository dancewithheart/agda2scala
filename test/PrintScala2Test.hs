module PrintScala2Test (printScala2Tests) where

import Agda.Compiler.Scala.PrintScala2 (
    combineLines,
    printCaseClass,
    printCaseObject,
    printPackageAndObject,
    printScala2,
    printSealedTrait,
    printSum,
 )
import Agda.Compiler.Scala.ScalaExpr (
    ScalaCtor (..),
    ScalaExpr (..),
    ScalaTerm (..),
    ScalaType (..),
    ScalaTypeScheme (..),
    SeVar (..),
 )
import Test.HUnit (Test (..), assertEqual)

testPrintCaseObject :: Test
testPrintCaseObject =
    TestCase $
        assertEqual
            "printCaseObject"
            "case object Light extends Color"
            (printCaseObject "Color" "Light")

testPrintSealedTrait :: Test
testPrintSealedTrait =
    TestCase $
        assertEqual
            "printSealedTrait"
            "sealed trait Color"
            (printSealedTrait "Color")

testObjectWhenNoPackage :: Test
testObjectWhenNoPackage =
    TestCase $
        assertEqual
            "printPackageAndObject"
            "object adts"
            (printPackageAndObject ["adts"])

testPrintPackageAndObject :: Test
testPrintPackageAndObject =
    TestCase $
        assertEqual
            "printPackageAndObject"
            "package example\n\nobject adts"
            (printPackageAndObject ["example", "adts"])

testPrintMultiplePartPackageAndObject :: Test
testPrintMultiplePartPackageAndObject =
    TestCase $
        assertEqual
            "printPackageAndObject"
            "package org.example\n\nobject adts"
            (printPackageAndObject ["org", "example", "adts"])

testCombineLines :: Test
testCombineLines =
    TestCase $
        assertEqual
            "combineLines"
            "a\nb"
            (combineLines ["", "a", "", "", "b", "", "", ""])

testPrintCaseClass :: Test
testPrintCaseClass =
    TestCase $
        assertEqual
            "printCaseClass"
            "final case class RgbPair(snd: Bool, fst: Rgb)"
            ( printCaseClass
                "RgbPair"
                []
                [ SeVar "snd" (STyName "Bool")
                , SeVar "fst" (STyName "Rgb")
                ]
            )

testPrintCaseClassPoly :: Test
testPrintCaseClassPoly =
    TestCase $
        assertEqual
            "printCaseClassPolymorphic"
            "final case class Box[A](unbox: A)"
            ( printCaseClass
                "Box"
                ["A"]
                [SeVar "unbox" (STyVar "A")]
            )

testPrintScala2 :: Test
testPrintScala2 =
    TestCase $
        assertEqual
            "printScala2"
            expected
            (printScala2 $ SePackage ["adts"] moduleContent)
  where
    rgbAdt =
        SeSum
            "Rgb"
            []
            [ ScalaCtor "Red" []
            , ScalaCtor "Green" []
            , ScalaCtor "Blue" []
            ]
    colorAdt =
        SeSum
            "Color"
            []
            [ ScalaCtor "Light" []
            , ScalaCtor "Dark" []
            ]
    blank = SeUnhandled "" ""
    moduleContent = [rgbAdt, blank, blank, blank, colorAdt, blank, blank]
    expected =
        "object adts\n"
            <> "{\n"
            <> "\n"
            <> "sealed trait Rgb\n"
            <> "object Rgb {\n"
            <> "  case object Red extends Rgb\n"
            <> "  case object Green extends Rgb\n"
            <> "  case object Blue extends Rgb\n"
            <> "\n"
            <> "}\n" -- this blank line is currently produced by your printer
            <> "\n"
            <> "sealed trait Color\n"
            <> "object Color {\n"
            <> "  case object Light extends Color\n"
            <> "  case object Dark extends Color\n"
            <> "\n"
            <> "}\n" -- likewise
            <> "}\n"

testPrintSum :: Test
testPrintSum =
    TestCase $
        assertEqual
            "testPrintSum"
            expected
            ( printSum
                "Rgb"
                []
                [ ScalaCtor "Red" []
                , ScalaCtor "Green" []
                , ScalaCtor "Blue" []
                ]
            )
  where
    expected =
        "sealed trait Rgb\n"
            <> "object Rgb {\n"
            <> "  case object Red extends Rgb\n"
            <> "  case object Green extends Rgb\n"
            <> "  case object Blue extends Rgb\n"
            <> "\n"
            <> "}"

testPrintSumPoly :: Test
testPrintSumPoly =
    TestCase $
        assertEqual
            "testPrintSumPoly"
            expected
            ( printSum
                "Maybe"
                ["A"]
                [ ScalaCtor "None" []
                , ScalaCtor "Just" [STyVar "A"]
                ]
            )
  where
    expected =
        "sealed trait Maybe[A]\n"
            <> "object Maybe {\n"
            <> "  case object None extends Maybe[Nothing]\n"
            <> "  final case class Just[A](x0: A) extends Maybe[A]\n"
            <> "\n"
            <> "}"

testPolyDef :: Test
testPolyDef =
    TestCase $
        assertEqual
            "prints def with [A] and return A"
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
printScala2Tests =
    TestList
        [ TestLabel "printCaseObject" testPrintCaseObject
        , TestLabel "printSealedTrait" testPrintSealedTrait
        , TestLabel "printObject" testObjectWhenNoPackage
        , TestLabel "printPackageAndObject" testPrintPackageAndObject
        , TestLabel "printPackageAndObject 2" testPrintMultiplePartPackageAndObject
        , TestLabel "combineLines" testCombineLines
        , TestLabel "printCaseClass" testPrintCaseClass
        , TestLabel "printCaseClassPolymorphic" testPrintCaseClassPoly
        , TestLabel "testPrintSum" testPrintSum
        , TestLabel "testPrintSumPoly" testPrintSumPoly
        , TestLabel "printScala2" testPrintScala2
        , TestLabel "printScala2 polymorphic def" testPolyDef
        ]
