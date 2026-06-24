module Render.PrintScala3Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Agda.Compiler.Scala.IR.ScalaExpr
    ( ScalaCtor (..)
    , ScalaExpr (..)
    , ScalaPat (..)
    , ScalaTerm (..)
    , ScalaType (..)
    , SeVar (..)
    , scalaTypeScheme
    )
import Agda.Compiler.Scala.Render.PrintScala3
    ( printCaseClass
    , printCaseObject
    , printPackageAndObject
    , printScala3
    , printSealedTrait
    )
import Support.Assertions (assertStringEqual)

tests :: TestTree
tests =
    testGroup
        "Render.Scala3"
        [ testGroup
            "module headers"
            [ testCase "prints object when module has one name part" test_printPackage
            ]
        , testGroup
            "declarations"
            [ testCase "prints sealed trait for an ADT parent" test_printSealedTrait
            , testCase "prints case object for a zero-argument constructor" test_printCaseObject
            , testCase "prints case class for a record/product type" test_printCaseClass
            ]
        , testGroup
            "layout"
            [ testCase "prints package with two enum declarations and skips empty unhandled declarations" test_printScala3Package
            ]
        , testGroup
            "pattern matching"
            [ testCase "prints flat constructor match" test_printMatch
            , testCase "prints flat constructor match with application on the right-hand side" test_printMatchWithAppRhs
            ]
         , testGroup
            "it then else"
            [ testCase "prints if then else" test_print_if_then_else
            ]
        ]

test_printCaseObject :: IO ()
test_printCaseObject =
    assertStringEqual
        "Scala 3 case object"
        "case object Light extends Color"
        (printCaseObject "Color" "Light")

test_printSealedTrait :: IO ()
test_printSealedTrait =
    assertStringEqual
        "Scala 3 sealed trait"
        "sealed trait Color"
        (printSealedTrait "Color")

test_printPackage :: IO ()
test_printPackage =
    assertStringEqual
        "Scala 3 object"
        "object adts"
        (printPackageAndObject ["adts"])

test_printCaseClass :: IO ()
test_printCaseClass =
    assertStringEqual
        "Scala 3 case class"
        "final case class RgbPair(snd: Bool, fst: Rgb)"
        ( printCaseClass
            "RgbPair"
            [ SeVar "snd" (STyName "Bool")
            , SeVar "fst" (STyName "Rgb")
            ]
        )

test_printScala3Package :: IO ()
test_printScala3Package =
    assertStringEqual
        "Scala 3 printer renders a package object with two enums and skips empty SeUnhandled declarations"
        expected
        (printScala3 $ SePackage ["adts"] moduleContent)
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
        "object adts:\n"
            <> "  enum Rgb:\n"
            <> "    case Red\n"
            <> "    case Green\n"
            <> "    case Blue\n"
            <> "\n"
            <> "  enum Color:\n"
            <> "    case Light\n"
            <> "    case Dark\n"

test_printMatch :: IO ()
test_printMatch =
    assertStringEqual
        "Scala 3 printer renders flat constructor match"
        expected
        (printScala3 expr)
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
        "def not(x0: Answer): Answer = x0 match\n"
            <> "  case Answer.Yes =>\n"
            <> "    Answer.No\n"
            <> "  case Answer.No =>\n"
            <> "    Answer.Yes\n"

test_printMatchWithAppRhs :: IO ()
test_printMatchWithAppRhs =
    assertStringEqual
        "Scala 3 printer renders flat constructor match with application RHS"
        expected
        (printScala3 expr)
  where
    expr =
        SeFun
            "normalize"
            [SeVar "x0" (STyName "Answer")]
            (scalaTypeScheme (STyName "Answer"))
            ( STeMatch
                (STeVar "x0")
                [ (SPCtor "Answer.Yes" [], STeApp (STeVar "wrap") [STeVar "Answer.No"])
                , (SPCtor "Answer.No" [], STeApp (STeVar "wrap") [STeVar "Answer.Yes"])
                ]
            )
    expected =
        "def normalize(x0: Answer): Answer = x0 match\n"
            <> "  case Answer.Yes =>\n"
            <> "    wrap(Answer.No)\n"
            <> "  case Answer.No =>\n"
            <> "    wrap(Answer.Yes)\n"

test_print_if_then_else :: IO ()
test_print_if_then_else =
  assertStringEqual "printScala3 if then else"
    expected
    (printScala3 expr)
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
    expected = ""
     <> "def choose(x: Long, y: Long): Long = if x < y then\n"
     <> "  x\n"
     <> "else\n"
     <> "  y\n"
