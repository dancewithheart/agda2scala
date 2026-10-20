module Render.PropertyTest (tests) where

import Data.List (isInfixOf)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Agda.Compiler.Scala.IR.ScalaExpr
import Agda.Compiler.Scala.Render.PropertyTest
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3Term)

tests :: TestTree
tests =
  testGroup "Generated property tests"
    [ testCase "compares translated expressions" $ do
        let law = ScalaProperty
              { spName = "addZeroLaw"
              , spArg = SeVar "x0" (STyName "Long")
              , spLeft = STeApp (STeVar "addZero") [STeVar "x0"]
              , spRight = STeVar "x0"
              }
            output = printPropertySuite printScala3Term ["examples", "Properties"] [law]
        assertBool
          "generates ZIO Test property"
          ("assertTrue((addZero(x0)) == (x0))" `isInfixOf` output)
        assertBool
          "generates a natural-number generator"
          ("Gen.long(0L, 1000000L)" `isInfixOf` output)
    ]
