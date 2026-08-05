module Render.CommonTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase)

import Agda.Compiler.Scala.IR.ScalaExpr (ScalaPat (..))
import Agda.Compiler.Scala.Render.Common (combineLines, printPat)
import Support.Assertions (assertStringEqual)

tests :: TestTree
tests = testGroup "Render.Common"
  [ testCase "combineLines removes empty lines and joins non-empty lines" test_combineLines
  , testCase
      "printPat parenthesizes a cons pattern in cons-head position"
      test_printPat_nestedConsHead
  ]

test_combineLines :: IO ()
test_combineLines =
    assertStringEqual
        "combined lines"
        "a\nb"
        (combineLines ["", "a", "", "", "b", "", "", ""])

test_printPat_nestedConsHead :: IO ()
test_printPat_nestedConsHead =
    assertStringEqual
        "nested cons head"
        "(x :: xs) :: xss"
        (printPat (SPCons (SPCons (SPVar "x") (SPVar "xs")) (SPVar "xss")))
