{-# LANGUAGE OverloadedStrings #-}

module Compile.TypesTest (tests) where

import Agda.Syntax.Common
    ( Arg (..)
    , defaultArgInfo
    )
import Agda.Syntax.Internal
    ( Abs (..)
    , Dom
    , Elim' (..)
    , Sort
    , Term (..)
    , Type
    , Type'' (..)
    , defaultDom
    )
import Test.Tasty
    ( TestTree
    , testGroup
    )
import Test.Tasty.HUnit
    ( Assertion
    , assertBool
    , assertFailure
    , testCase
    , (@?=)
    )

import Agda.Compiler.Scala.Compile.Types
    ( compileTypeTermWith
    , isTypeLike
    , lookupTyVar
    , pushTermBinder
    , pushTyParam
    )
import Agda.Compiler.Scala.IR.ScalaExpr
    ( ScalaType (..)
    )

tests :: TestTree
tests =
    testGroup
        "Compile.Types"
        [ testGroup
            "type-like binder heuristic"
            [ testCase "Sort is treated as a type-level binder" test_isTypeLike_sort
            , testCase "Pi ending in Sort is treated as a higher-kinded type-level binder" test_isTypeLike_piToSort
            ]
        , testGroup
            "type variable environment"
            [ testCase "lookupTyVar resolves the most recent type binder" test_lookupTyVar_resolvesMostRecent
            , testCase "lookupTyVar falls back to t<i> for missing binders" test_lookupTyVar_missingBinderFallback
            , testCase "lookupTyVar falls back to t<i> for term binders" test_lookupTyVar_termBinderFallback
            ]
        , testGroup
            "type term lowering"
            [ testCase "compileTypeTermWith lowers a type variable to STyVar" test_compileTypeTermWith_typeVariable
            , testCase "compileTypeTermWith lowers applied type variable to STyApp" test_compileTypeTermWith_typeVariableApplication
            , testCase "compileTypeTermWith rejects unsupported lambda terms explicitly" test_compileTypeTermWith_rejectsLambda
            ]
        ]

-- A : Type u  ==> El _ (Sort _)
--
-- This intentionally uses error payloads. The heuristic should inspect only
-- the outer Agda constructors and must not force sort annotations.
test_isTypeLike_sort :: IO ()
test_isTypeLike_sort =
    assertBool
        "Sort should be recognized as a type-level binder"
        (isTypeLike typeSort)

typeSort :: Type
typeSort = El dummySort (Sort dummySort)

dummySort :: Sort
dummySort =
    error "dummy Sort payload should not be forced"

-- F : Type u -> Type u  ==> El _ (Pi _ -> Sort _)
--
-- This documents the higher-kinded type parameter heuristic.
test_isTypeLike_piToSort :: IO ()
test_isTypeLike_piToSort =
    assertBool
        "Pi ending in Sort should be recognized as a higher-kinded type-level binder"
        (isTypeLike piToSortType)

piToSortType :: Type
piToSortType = El dummySort (
  Pi (dummyDom typeSort) (Abs "A" typeSort))

dummyDom :: Type -> Dom Type
dummyDom = defaultDom

test_lookupTyVar_resolvesMostRecent :: Assertion
test_lookupTyVar_resolvesMostRecent = do
    let env = pushTyParam "A" mempty

    lookupTyVar env 0 @?= "A"

test_lookupTyVar_missingBinderFallback :: Assertion
test_lookupTyVar_missingBinderFallback = do
    let env = mempty

    lookupTyVar env 0 @?= "t0"
    lookupTyVar env 3 @?= "t3"

test_lookupTyVar_termBinderFallback :: Assertion
test_lookupTyVar_termBinderFallback = do
    let env = pushTermBinder mempty

    lookupTyVar env 0 @?= "t0"

test_compileTypeTermWith_typeVariable :: Assertion
test_compileTypeTermWith_typeVariable = do
    let env = pushTyParam "A" mempty
        term = Var 0 []

    compileTypeTermWith env term @?= Right (STyVar "A")

test_compileTypeTermWith_typeVariableApplication :: Assertion
test_compileTypeTermWith_typeVariableApplication = do
    let env = pushTyParam "A" mempty
        term = Var 0 [mkApply (Var 0 [])]

    compileTypeTermWith env term @?= Right (STyApp "A" [STyVar "A"])

test_compileTypeTermWith_rejectsLambda :: Assertion
test_compileTypeTermWith_rejectsLambda = do
    let actual = compileTypeTermWith mempty unsupportedLambdaTerm

    assertLeft "lambda is not a supported type term" actual

mkApply :: Term -> Elim' Term
mkApply term =
    Apply (Arg defaultArgInfo term)

unsupportedLambdaTerm :: Term
unsupportedLambdaTerm =
    Lam defaultArgInfo (Abs "x" (Var 0 []))

assertLeft :: (Show a) => String -> Either e a -> Assertion
assertLeft label actual =
    case actual of
        Left _ ->
            pure ()
        Right value ->
            assertFailure $
                unlines
                    [ label
                    , ""
                    , "Expected:"
                    , "  Left _"
                    , ""
                    , "Actual:"
                    , "  " <> show value
                    ]
