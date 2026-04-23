module NamePolicyTest (tests) where

import Test.HUnit (Test(..), assertEqual)
import Agda.Compiler.Scala.NamePolicy (defaultNamePolicy, ctorName)

tests :: Test
tests = TestList
  [ TestLabel "ctor [] -> Nil" $
      TestCase (assertEqual "[]" "Nil" (ctorName defaultNamePolicy "[]"))
  , TestLabel "ctor _cons_ -> Cons" $
      TestCase (assertEqual "_cons_" "Cons" (ctorName defaultNamePolicy "_cons_"))
  ]
