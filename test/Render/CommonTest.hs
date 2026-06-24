module Render.CommonTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase)

import Agda.Compiler.Scala.Render.Common (combineLines)
import Support.Assertions (assertStringEqual)

tests :: TestTree
tests = testGroup "Render.Common"
  [ testCase "combineLines removes empty lines and joins non-empty lines" test_combineLines
  ]

test_combineLines :: IO ()
test_combineLines =
    assertStringEqual
        "combined lines"
        "a\nb"
        (combineLines ["", "a", "", "", "b", "", "", ""])
