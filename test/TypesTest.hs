module TypesTest (tests) where

import Test.HUnit (Test (..), assertBool)

import Agda.Compiler.Scala.AgdaToScalaExpr.Types (isTypeLike)
import Agda.Syntax.Internal (Term (..), Type, Type'' (..))

tests :: Test
tests = TestLabel "TypesTest" (TestList [test_isTypeLike_sort, test_isTypeLike_piToSort])

-- A : Type u  ==> El _ (Sort _)
test_isTypeLike_sort :: Test
test_isTypeLike_sort =
    TestCase $
        assertBool "Sort is type-like" (isTypeLike (El (error "sort-ann") (Sort (error "Sort payload"))))

-- F : Type u -> Type u ==> El _ (Pi _ -> Sort _)
test_isTypeLike_piToSort :: Test
test_isTypeLike_piToSort =
    TestCase $
        assertBool
            "Pi ... -> Sort is type-like"
            (isTypeLike (El (error "sort-ann") (Pi (error "dom") (error "abs"))))
