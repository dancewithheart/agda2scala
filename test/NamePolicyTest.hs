module NamePolicyTest (namePolicytests) where

import Agda.Compiler.Scala.NamePolicy (ctorName, defaultNamePolicy)
import Test.HUnit (Test (..), assertEqual)

namePolicytests :: Test
namePolicytests =
    TestList
        [ TestLabel "ctor [] -> Nil" $
            TestCase (assertEqual "[]" "Nil" (ctorName defaultNamePolicy "[]"))
        , TestLabel "ctor _cons_ -> Cons" $
            TestCase (assertEqual "_cons_" "_cons_" (ctorName defaultNamePolicy "_cons_"))
        ]
