module Render.CommonTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase)

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaPat (..)
  , ScalaType (..)
  )
import Agda.Compiler.Scala.Render.Common
  ( combineLines
  , printPat
  , printType
  )
import Support.Assertions (assertStringEqual)

tests :: TestTree
tests = testGroup "Render.Common"
  [ testCase "combineLines removes empty lines and joins non-empty lines" test_combineLines
  , testCase
      "printPat parenthesizes a cons pattern in cons-head position"
      test_printPat_nestedConsHead
  , testCase "printType prints function result" test_printType_functionResult
  , testCase "printType prints function input" test_printType_functionInput
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

test_printType_functionResult :: IO ()
test_printType_functionResult =
    assertStringEqual
        "function result"
        "A => B => C"
        ( printType
            (STyFun
                (STyVar "A")
                (STyFun
                    (STyVar "B")
                    (STyVar "C")
                )
            )
        )

test_printType_functionInput :: IO ()
test_printType_functionInput =
    assertStringEqual
        "function input"
        "(A => B) => C"
        ( printType
            (STyFun
                (STyFun
                    (STyVar "A")
                    (STyVar "B")
                )
                (STyVar "C")
            )
        )
