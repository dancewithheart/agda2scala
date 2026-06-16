module Name.NamePolicyTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Agda.Compiler.Scala.Name.NamePolicy
    ( ctorName
    , defaultNamePolicy
    )

tests :: TestTree
tests =
    testGroup
        "Name.NamePolicy / Agda constructor names"
        [ testCase "constructor [] is rendered as Nil" $
            assertEqual "Scala constructor name" "Nil" (ctorName defaultNamePolicy "[]")
        , testCase "constructor _::_ is rendered as Cons" $
            assertEqual "Scala constructor name" "Cons" (ctorName defaultNamePolicy "_::_")
        , testCase "constructor _∷_ is rendered as Cons" $
            assertEqual "Scala constructor name" "Cons" (ctorName defaultNamePolicy "_∷_")
        , testCase "constructor _,_ is rendered as Pair" $
            assertEqual "Scala constructor name" "Pair" (ctorName defaultNamePolicy "_,_")
        , testCase "constructor _cons_ is not rewritten by the default policy" $
            assertEqual "Scala constructor name" "_cons_" (ctorName defaultNamePolicy "_cons_")
        ]
