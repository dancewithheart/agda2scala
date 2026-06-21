module Render.PrintScala2Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Agda.Compiler.Scala.IR.ScalaExpr
    ( ScalaCtor (..)
    , ScalaExpr (..)
    , ScalaPat (..)
    , ScalaTerm (..)
    , ScalaType (..)
    , ScalaTypeScheme (..)
    , SeVar (..)
    , scalaTypeScheme
    )
import Agda.Compiler.Scala.Render.PrintScala2
    ( combineLines
    , printCaseClass
    , printCaseObject
    , printPackageAndObject
    , printScala2
    , printSealedTrait
    , printSum
    )
import Support.Assertions (assertStringEqual)

tests :: TestTree
tests =
    testGroup
        "Render.Scala2"
        [ testGroup
            "module headers"
            [ testCase "prints object when module has one name part" test_objectWhenNoPackage
            , testCase "prints package and object for two-part module name" test_packageAndObject
            , testCase "prints dotted package and object for multi-part module name" test_multiplePartPackageAndObject
            ]
        , testGroup
            "declarations"
            [ testCase "prints sealed trait for an ADT parent" test_printSealedTrait
            , testCase "prints case object for a zero-argument constructor" test_printCaseObject
            , testCase "prints case class for a record/product type" test_printCaseClass
            , testCase "prints polymorphic case class" test_printCaseClassPoly
            , testCase "prints monomorphic sum type" test_printSum
            , testCase "prints polymorphic sum type" test_printSumPoly
            , testCase "prints polymorphic function definition" test_polyDef
            ]
        , testGroup
            "layout"
            [ testCase "combineLines removes empty lines and joins non-empty lines" test_combineLines
            , testCase "prints package with handled declarations and skips empty unhandled declarations" test_printScala2Package
            ]
        , testGroup
            "pattern matching"
            [ testCase "prints flat constructor match" test_printMatch
            ]
         , testGroup
            "it then else"
            [ testCase "prints if then else" test_print_if_then_else
            ]
        ]

test_printCaseObject :: IO ()
test_printCaseObject =
    assertEqual
        "Scala 2 case object"
        "case object Light extends Color"
        (printCaseObject "Color" "Light")

test_printSealedTrait :: IO ()
test_printSealedTrait =
    assertEqual
        "Scala 2 sealed trait"
        "sealed trait Color"
        (printSealedTrait "Color")

test_objectWhenNoPackage :: IO ()
test_objectWhenNoPackage =
    assertEqual
        "Scala 2 object"
        "object adts"
        (printPackageAndObject ["adts"])

test_packageAndObject :: IO ()
test_packageAndObject =
    assertEqual
        "Scala 2 package and object"
        "package example\n\nobject adts"
        (printPackageAndObject ["example", "adts"])

test_multiplePartPackageAndObject :: IO ()
test_multiplePartPackageAndObject =
    assertEqual
        "Scala 2 dotted package and object"
        "package org.example\n\nobject adts"
        (printPackageAndObject ["org", "example", "adts"])

test_combineLines :: IO ()
test_combineLines =
    assertEqual
        "combined lines"
        "a\nb"
        (combineLines ["", "a", "", "", "b", "", "", ""])

test_printCaseClass :: IO ()
test_printCaseClass =
    assertEqual
        "Scala 2 case class"
        "final case class RgbPair(snd: Bool, fst: Rgb)"
        ( printCaseClass
            "RgbPair"
            []
            [ SeVar "snd" (STyName "Bool")
            , SeVar "fst" (STyName "Rgb")
            ]
        )

test_printCaseClassPoly :: IO ()
test_printCaseClassPoly =
    assertEqual
        "Scala 2 polymorphic case class"
        "final case class Box[A](unbox: A)"
        ( printCaseClass
            "Box"
            ["A"]
            [SeVar "unbox" (STyVar "A")]
        )

test_printScala2Package :: IO ()
test_printScala2Package =
    assertStringEqual
        "Scala 2 printer renders a package object with two ADTs and skips empty SeUnhandled declarations"
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
            <> "}\n"
            <> "\n"
            <> "sealed trait Color\n"
            <> "object Color {\n"
            <> "  case object Light extends Color\n"
            <> "  case object Dark extends Color\n"
            <> "\n"
            <> "}\n"
            <> "}\n"

test_printSum :: IO ()
test_printSum =
    assertStringEqual
        "Scala 2 printer renders a monomorphic ADT as sealed trait plus companion object"
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

test_printSumPoly :: IO ()
test_printSumPoly =
    assertStringEqual
        "Scala 2 printer renders a polymorphic ADT with Nothing for zero-argument constructors"
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

test_polyDef :: IO ()
test_polyDef =
    assertStringEqual
        "Scala 2 printer renders type parameters on polymorphic functions"
        expected
        (printScala2 expr)
  where
    expr =
        SeFun
            "id"
            [SeVar "x1" (STyVar "A")]
            (ScalaTypeScheme ["A"] (STyVar "A"))
            (STeVar "x1")
    expected = "def id[A](x1: A): A = x1\n"

test_printMatch :: IO ()
test_printMatch =
    assertStringEqual
        "Scala 2 printer renders flat constructor match"
        expected
        (printScala2 expr)
  where
    expr =
        SeFun
            "not"
            [SeVar "x0" (STyName "Answer")]
            (scalaTypeScheme (STyName "Answer"))
            ( STeMatch
                (STeVar "x0")
                [ (SPCtor "Answer.Yes" [], STeVar "Answer.No")
                , (SPCtor "Answer.No" [], STeVar "Answer.Yes")
                ]
            )
    expected =
        "def not(x0: Answer): Answer = x0 match {\n"
            <> "  case Answer.Yes => Answer.No\n"
            <> "  case Answer.No => Answer.Yes\n"
            <> "}\n"

test_print_if_then_else :: IO ()
test_print_if_then_else =
  assertStringEqual "printScala2 if then else"
    expected
    (printScala2 expr)
  where
    expr =
      SeFun
        "choose"
        [ SeVar "x" (STyName "Long")
        , SeVar "y" (STyName "Long")
        ]
        (scalaTypeScheme (STyName "Long"))
        ( STeIf
            (STeBinOp (STeVar "x") "<" (STeVar "y"))
            (STeVar "x")
            (STeVar "y")
        )
    expected =
      "def choose(x: Long, y: Long): Long = if (x < y) x else y\n"