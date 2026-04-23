module PrintScala3Test ( printScala3Tests ) where

import Test.HUnit ( Test(..), assertEqual )
import Agda.Compiler.Scala.PrintScala3
  ( printScala3
  , printSealedTrait
  , printCaseObject
  , printPackageAndObject
  , combineLines
  , printCaseClass
  )
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaExpr(..)
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

testPrintPackage :: Test
testPrintPackage = TestCase $
  assertEqual "printPackageAndObject"
    "object adts"
    (printPackageAndObject ["adts"])

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

testPrintScala3 :: Test
testPrintScala3 = TestCase $
  assertEqual "printScala3"
    expected
    (printScala3 $ SePackage ["adts"] moduleContent)
  where
    rgbAdt =
      SeSum "Rgb" []
        [ ScalaCtor "Red"   []
        , ScalaCtor "Green" []
        , ScalaCtor "Blue"  []
        ]
    colorAdt =
      SeSum "Color" []
        [ ScalaCtor "Light" []
        , ScalaCtor "Dark"  []
        ]
    blank = SeUnhandled "" ""
    moduleContent = [rgbAdt, blank, blank, blank, colorAdt, blank, blank]
    expected =
      "object adts:\n" <>
      "  enum Rgb:\n" <>
      "    case Red\n" <>
      "    case Green\n" <>
      "    case Blue\n" <>
      "\n" <>
      "  enum Color:\n" <>
      "    case Light\n" <>
      "    case Dark\n"

printScala3Tests :: Test
printScala3Tests = TestList
  [ TestLabel "printCaseObject" testPrintCaseObject
  , TestLabel "printSealedTrait" testPrintSealedTrait
  , TestLabel "printPackage" testPrintPackage
  , TestLabel "combineLines" testCombineLines
  , TestLabel "printCaseClass" testPrintCaseClass
  , TestLabel "printScala3" testPrintScala3
  ]
