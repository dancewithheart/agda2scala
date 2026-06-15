{-# LANGUAGE OverloadedStrings #-}

module TermsProps (tests) where

import Hedgehog (Group(..), Property, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.AgdaToScalaExpr.Terms (compileBodyTerm, Env(..))
import Agda.Compiler.Scala.ScalaExpr (ScalaTerm(..))
import Agda.Compiler.Scala.AgdaToScalaExpr.Types (CompileError)

import Agda.Syntax.Common (Arg, defaultArg)
import Agda.Syntax.Internal (Term(..), Elim'(..))

tests :: Group
tests =
  Group "Terms / application"
    [ ("Def with k Apply args compiles to application of arity k", prop_def_apply_arity)
    , ("pattern vars extend Env with newest binder at index 0", prop_extendEnv_patternVarsNewestFirst)
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

prop_extendEnv_patternVarsNewestFirst :: Property
prop_extendEnv_patternVarsNewestFirst = property $ do
  arity <- forAll (Gen.int (Range.linear 0 8))
  let patVars = [ "p" <> show i | i <- [0 .. arity - 1] ]
      env     = extendEnv patVars (Env ["old"])
  traverse_
    (\(i, expected) -> lookupVar env i === Right expected)
    (zip [0 ..] (reverse patVars))
