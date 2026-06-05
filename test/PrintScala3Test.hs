module PrintScala3Test (printScala3Tests) where

import Agda.Compiler.Scala.PrintScala3 (
    combineLines,
    printCaseClass,
    printCaseObject,
    printPackageAndObject,
    printScala3,
    printSealedTrait,
 )
import Agda.Compiler.Scala.ScalaExpr (
    ScalaCtor (..),
    ScalaPat (..),
    ScalaExpr (..),
    ScalaTerm (..),
    ScalaType (..),
    SeVar (..),
    scalaTypeScheme
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

testPrintPackage :: Test
testPrintPackage =
    TestCase $
        assertEqual
            "printPackageAndObject"
            "object adts"
            (printPackageAndObject ["adts"])

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
                [ SeVar "snd" (STyName "Bool")
                , SeVar "fst" (STyName "Rgb")
                ]
            )

testPrintScala3 :: Test
testPrintScala3 =
    TestCase $
        assertEqual
            "printScala3"
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

testPrintMatch :: Test
testPrintMatch = TestCase $
  assertEqual "printScala3 match"
    expected
    (printScala3 expr)
  where
    expr =
      SeFun
        "not"
        [SeVar "x0" (STyName "Answer")]
        (scalaTypeScheme (STyName "Answer"))
        (STeMatch
          (STeVar "x0")
          [ (SPCtor "Yes" [], STeVar "No")
          , (SPCtor "No" [], STeVar "Yes")
          ])

    expected =
      "def not(x0: Answer): Answer = x0 match\n" <>
      "  case Yes => No\n" <>
      "  case No => Yes\n"

printScala3Tests :: Test
printScala3Tests =
    TestList
        [ TestLabel "printCaseObject" testPrintCaseObject
        , TestLabel "printSealedTrait" testPrintSealedTrait
        , TestLabel "printPackage" testPrintPackage
        , TestLabel "combineLines" testCombineLines
        , TestLabel "printCaseClass" testPrintCaseClass
        , TestLabel "printScala3" testPrintScala3
        , TestLabel "testPrintMatch" testPrintMatch
        ]
