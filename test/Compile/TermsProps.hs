{-# LANGUAGE OverloadedStrings #-}

module Compile.TermsProps (tests) where

import Data.Foldable (traverse_)
import Hedgehog
  ( Group(..)
  , Property
  , annotateShow
  , failure
  , forAll
  , property
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Agda.Syntax.Common (defaultArg)
import Agda.Syntax.Internal (Elim' (..), Term (..))
import Agda.Compiler.Scala.Compile.Terms
  ( Env(..)
  , envFromArgs
  , extendEnv
  , lookupVar
  )
import Agda.Compiler.Scala.Compile (compileBodyTerm)
import Agda.Compiler.Scala.Compile.Types (CompileError(..))
import Agda.Compiler.Scala.IR.ScalaExpr (ScalaTerm(..))

tests :: Group
tests =
  Group "Terms / Env / application"
    [ ("Def with k Apply args compiles to application of arity k", prop_def_apply_arity)
    , ("pattern vars extend Env with newest binder at index 0", prop_extendEnv_patternVarsNewestFirst)
    , ("function args enter Env with last arg at index 0", prop_envFromArgs_functionArgsNewestFirst)
    , ("lookupVar reports index and Env size when out of range", prop_lookupVar_outOfRange)
    ]

mkApply :: Term -> Elim' Term
mkApply t = Apply (defaultArg t)

prop_def_apply_arity :: Property
prop_def_apply_arity = property $ do
  k <- forAll (Gen.int (Range.linear 0 5))

  let env   = Env ["x0"]
      elims = replicate k (mkApply (Var 0 []))
      t     = Var 0 elims

  case compileBodyTerm env t of
    Left _ -> do
      -- acceptable only if k==0 and variable resolves; otherwise fail
      k === 0
    Right tm ->
      case tm of
        STeVar _      -> k === 0
        STeApp _ args -> length args === k
        _             -> do
          annotateShow tm
          failure

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
