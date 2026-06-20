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
import Agda.Syntax.Common ( NameId(..), defaultArg)
import Agda.Syntax.Internal (Elim' (..), Term (..))
import Agda.Syntax.TopLevelModuleName.Boot ( noModuleNameHash )
import Agda.Compiler.Scala.Compile.Terms
  ( Env(..)
  , envFromArgs
  , extendEnv
  , lookupVar
  )
import Agda.Compiler.Scala.Compile (compileBodyTerm)
import Agda.Compiler.Scala.Compile.Types (CompileError(..))
import Agda.Compiler.Scala.IR.ScalaExpr (ScalaTerm(..))

termsProps :: Group
termsProps =
  Group "Terms / Env / application"
    [ ("Def with k Apply args compiles to application of arity k", prop_def_apply_arity)
    , ("pattern vars extend Env with newest binder at index 0", prop_extendEnv_patternVarsNewestFirst)
    , ("function args enter Env with last arg at index 0", prop_envFromArgs_functionArgsNewestFirst)
    , ("lookupVar reports index and Env size when out of range", prop_lookupVar_outOfRange)
    ]

mkApply :: Term -> Elim' Term
mkApply t = Apply (defaultArg t)

-- For this generated subset, compileBodyTerm must succeed.
-- If k == 0, Def lowers to STeVar.
-- If k > 0, Def lowers to STeApp with exactly k arguments.
prop_def_apply_arity :: Property
prop_def_apply_arity = property $ do
  k <- forAll (Gen.int (Range.linear 0 8))

  let env = Env ["x"]
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
  let vars = [ "x" <> show i | i <- [0 .. size - 1] ]
      env  = Env vars
      ix   = size + extra
  lookupVar env ix === Left (VarOutOfRange ix size)
