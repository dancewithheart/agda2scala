{-# LANGUAGE OverloadedStrings #-}

module Compile.TermsProps (termsProps) where

import Data.Foldable (traverse_)
import Hedgehog
  ( Group(..)
  , Property
  , PropertyT
  , annotateShow
  , failure
  , forAll
  , property
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Agda.Syntax.Abstract.Name ( QName, mkName_ , qualify_ )
import Agda.Syntax.Common ( Arg, Hiding(..), NameId(..), defaultArg, setHiding )
import Agda.TypeChecking.CompiledClause( CompiledClauses'(..), catchall)
import Agda.Syntax.Internal (Elim' (..), Term (..))
import Agda.Syntax.TopLevelModuleName.Boot ( noModuleNameHash )
import Agda.Compiler.Scala.Compile.Terms
  ( Env(..)
  , compileFunctionBody
  , envFromArgs
  , envFromFunction
  , extendEnv
  , lookupCaseArg
  , lookupVar
  , removeCaseArg
  )
import Agda.Compiler.Scala.Compile (compileBodyTerm)
import Agda.Compiler.Scala.Compile.Types (CompileError(..))
import Agda.Compiler.Scala.IR.ScalaExpr (ScalaTerm(..), ScalaPat(..))

termsProps :: Group
termsProps =
  Group "Terms / Env / application"
    [ ("Def with k Apply args compiles to application of arity k", prop_def_apply_arity)
    , ("pattern vars extend Env with newest binder at index 0", prop_extendEnv_patternVarsNewestFirst)
    , ("function args enter Env with last arg at index 0", prop_envFromArgs_functionArgsNewestFirst)
    , ("lookupVar reports index and Env size when out of range", prop_lookupVar_outOfRange)
    , ("hidden application arguments do not become runtime Scala arguments", prop_hiddenApplyArgs_areErased)
    , ("function type params stay in Env as erased de Bruijn slots",
       prop_envFromFunction_keepsTypeParamsForDeBruijnAlignment)
    , ("case arguments use source-order positions including erased binders", prop_lookupCaseArg_usesSourceOrder)
    , ("case branch environment removes the scrutinized argument", prop_removeCaseArg_dropsScrutinee)
    , ("constructor branch binders are added after dropping the case scrutinee", prop_caseBranchEnv_addsPatternBindersAfterDroppingScrutinee)
    , ("catch-all branches retain the scrutinized runtime argument", prop_catchallBranch_retainsScrutinee)
    ]

mkApply :: Term -> Elim' Term
mkApply t = Apply (defaultArg t)

-- For this generated subset, compileBodyTerm must succeed.
-- If k == 0, Def lowers to STeVar.
-- If k > 0, Def lowers to STeApp with exactly k arguments.
prop_def_apply_arity :: Property
prop_def_apply_arity = property $ do
  k <- forAll (Gen.int (Range.linear 0 8))
  let env = Env [Just "x"]
      t =
        Def
          dummyQName
          (replicate k (mkApply (Var 0 [])))
  tm <- evalEither (compileBodyTerm env t)
  case tm of
    STeVar _ -> k === 0
    STeApp _ args -> length args === k
    other -> do
      annotateShow other
      failure

evalEither :: (Show e) => Either e a -> PropertyT IO a
evalEither x = case x of
    Right a -> pure a
    Left e -> do
      annotateShow e
      failure

dummyQName :: QName
dummyQName =
  qualify_ (mkName_ (NameId 0 noModuleNameHash) ("f" :: String))

-- f x0 x1 x2 becomes Env ["x2", "x1", "x0"]
-- so
-- Var 0 -> x2
-- Var 1 -> x1
-- Var 2 -> x0
prop_envFromArgs_functionArgsNewestFirst :: Property
prop_envFromArgs_functionArgsNewestFirst = property $ do
  argCount <- forAll (Gen.int (Range.linear 1 12))
  let args = [ "x" <> show i | i <- [0 .. argCount - 1] ]
      env  = envFromArgs args
  traverse_
    (\(i, expected) -> lookupVar env i === Right expected)
    (zip [0 ..] (reverse args))

prop_extendEnv_patternVarsNewestFirst :: Property
prop_extendEnv_patternVarsNewestFirst = property $ do
  oldCount <- forAll (Gen.int (Range.linear 0 8))
  patCount <- forAll (Gen.int (Range.linear 1 8))
  let oldVars = [ "x" <> show i | i <- [0 .. oldCount - 1] ]
      patVars = [ "p" <> show i | i <- [0 .. patCount - 1] ]
      oldEnv = envFromArgs oldVars
      env    = extendEnv patVars oldEnv
  traverse_
    (\(i, expected) -> lookupVar env i === Right expected)
    (zip [0 ..] (reverse patVars))
  traverse_
    (\(i, expected) -> lookupVar env (patCount + i) === Right expected)
    (zip [0 ..] (reverse oldVars))

prop_lookupVar_outOfRange :: Property
prop_lookupVar_outOfRange = property $ do
  size <- forAll (Gen.int (Range.linear 0 12))
  extra <- forAll (Gen.int (Range.linear 0 12))
  let vars = [ Just ("x" <> show i) | i <- [0 .. size - 1] ]
      env  = Env vars
      ix   = size + extra
  lookupVar env ix === Left (VarOutOfRange ix size)

-- Type parameters are erased at runtime, but preserved as empty slots in the de Bruijn environment:
-- Term Env preserves Agda de Bruijn indexing.
-- Visible term binders are stored as Just ScalaName.
-- Erased type-level binders are stored as Nothing.
-- This lets us erase type arguments from generated Scala while keeping Agda variable indices correct.
prop_envFromFunction_keepsTypeParamsForDeBruijnAlignment :: Property
prop_envFromFunction_keepsTypeParamsForDeBruijnAlignment = property $ do
  tyCount <- forAll (Gen.int (Range.linear 1 5))
  argCount <- forAll (Gen.int (Range.linear 1 8))

  let tyParams = [ "A" <> show i | i <- [0 .. tyCount - 1] ]
      args = [ "x" <> show i | i <- [0 .. argCount - 1] ]
      env = envFromFunction tyParams args

  traverse_
    (\(i, expected) -> lookupVar env i === Right expected)
    (zip [0 ..] (reverse args))

  lookupVar env argCount === Left (ErasedVarReferenced argCount)

prop_lookupCaseArg_usesSourceOrder :: Property
prop_lookupCaseArg_usesSourceOrder = property $ do
    let env = Env [Just "tree", Just "key", Just "defaultVal", Nothing]

    lookupCaseArg env 1 === Right "defaultVal"
    lookupCaseArg env 2 === Right "key"
    lookupCaseArg env 3 === Right "tree"

prop_removeCaseArg_dropsScrutinee :: Property
prop_removeCaseArg_dropsScrutinee = property $ do
    let env = Env [Just "tree", Just "key", Just "defaultVal", Nothing]

    branchEnv <- evalEither (removeCaseArg env 3)

    lookupVar branchEnv 0 === Right "key"
    lookupVar branchEnv 1 === Right "defaultVal"
    lookupVar branchEnv 2 === Left (ErasedVarReferenced 2)

prop_caseBranchEnv_addsPatternBindersAfterDroppingScrutinee :: Property
prop_caseBranchEnv_addsPatternBindersAfterDroppingScrutinee = property $ do
    let env = Env [Just "tree", Just "key", Just "defaultVal", Nothing]
        patVars = ["p0", "p1", "p2", "p3", "p4"]

    branchEnv <- evalEither (removeCaseArg env 3)
    let ctorEnv = extendEnv patVars branchEnv

    lookupVar ctorEnv 0 === Right "p4"
    lookupVar ctorEnv 3 === Right "p1"
    lookupVar ctorEnv 5 === Right "key"
    lookupVar ctorEnv 6 === Right "defaultVal"

prop_hiddenApplyArgs_areErased :: Property
prop_hiddenApplyArgs_areErased = property $ do
    let env = envFromFunction ["A"] ["x"]
        term = Def
          dummyQName
          [ Apply (hiddenArg (Var 1 []))
          , Apply (visibleArg (Var 0 []))
          ]
        expected =
            STeApp
                (STeVar "f")
                [STeVar "x"]
    compileBodyTerm env term === Right expected

visibleArg :: Term -> Arg Term
visibleArg = defaultArg

hiddenArg :: Term -> Arg Term
hiddenArg = setHiding Hidden . defaultArg

{-
Checks:
- case uses source-order indexing
- the wildcard RHS still resolves the scrutinized variable
- hidden type parameters do not disturb either index
-}
prop_catchallBranch_retainsScrutinee :: Property
prop_catchallBranch_retainsScrutinee = property $ do
    tyCount <- forAll (Gen.int (Range.linear 0 3))
    argCount <- forAll (Gen.int (Range.linear 1 8))
    runtimeIndex <- forAll (Gen.int (Range.linear 0 (argCount - 1)))

    let tyParams = [ "A" <> show i | i <- [0 .. tyCount - 1] ]
        args = [ "x" <> show i | i <- [0 .. argCount - 1] ]
        caseIndex = tyCount + runtimeIndex
        deBruijnIndex = argCount - runtimeIndex - 1
        scrutinizedName = args !! runtimeIndex
        clauses =
          Case
            (defaultArg caseIndex)
            (catchall (Done [] (Var deBruijnIndex [])))
        expected =
          STeMatch
            (STeVar scrutinizedName)
            [(SPWild, STeVar scrutinizedName)]

    compileFunctionBody tyParams args (Just clauses) === Right expected

