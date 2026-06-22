module Render.CommonTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Agda.Compiler.Scala.Render.Common (combineLines)

tests :: TestTree
tests = testGroup "combineLines"
  [ testCase "combineLines removes empty lines and joins non-empty lines" test_combineLines
  ]

test_combineLines :: IO ()
test_combineLines =
    assertEqual
        "combined lines"
        "a\nb"
        (combineLines ["", "a", "", "", "b", "", "", ""])
