module Render.PropertyTest (tests) where

import Data.List (isInfixOf)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Agda.Compiler.Scala.IR.ScalaExpr
import Agda.Compiler.Scala.Render.PropertyTest
import Agda.Compiler.Scala.Render.PrintScala2 (printScala2Term)
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3Term)

tests :: TestTree
tests =
  testGroup "Generated property tests"
    [ testCase "compares translated expressions" $ do
        let law = ScalaProperty
              { spName = "addZeroLaw"
              , spArgs = [SeVar "x0" (STyName "Long")]
              , spLeft = STeApp (STeVar "addZero") [STeVar "x0"]
              , spRight = STeVar "x0"
              }
            output =
              printPropertySuite
                printScala3Term
                ["examples", "Properties"]
                [law]
        assertBool
          "generates ZIO Test property"
          ("assertTrue((addZero(x0)) == (x0))" `isInfixOf` output)
        assertBool
          "generates a natural-number generator"
          ("Gen.long(0L, 1000000L)" `isInfixOf` output)

    , testCase "generates three-argument associativity property" $ do
        let x = STeVar "x0"
            y = STeVar "x1"
            z = STeVar "x2"
            law = ScalaProperty
              { spName = "natPlusAssoc"
              , spArgs =
                  [ SeVar "x0" (STyName "Long")
                  , SeVar "x1" (STyName "Long")
                  , SeVar "x2" (STyName "Long")
                  ]
              , spLeft =
                  STeBinOp (STeBinOp x "+" y) "+" z
              , spRight =
                  STeBinOp x "+" (STeBinOp y "+" z)
              }
            output3 =
              printPropertySuite
                printScala3Term
                ["examples", "Monoids"]
                [law]
            output2 =
              printPropertySuite
                printScala2Term
                ["examples", "Monoids"]
                [law]
            expected =
              "((x0 + x1) + x2) == (x0 + (x1 + x2))"
        assertBool
          "Scala 3 preserves associativity"
          (expected `isInfixOf` output3)
        assertBool
          "Scala 2 preserves associativity"
          (expected `isInfixOf` output2)
        assertBool
          "generates three generators"
          ( "Gen.long(0L, 1000000L), Gen.long(0L, 1000000L), Gen.long(0L, 1000000L)"
              `isInfixOf` output3
          )
        assertBool
          "generates three binders"
          ("(x0, x1, x2) =>" `isInfixOf` output3)
    ]
