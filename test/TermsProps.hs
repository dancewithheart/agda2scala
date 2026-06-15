{-# LANGUAGE OverloadedStrings #-}

module TermsProps (tests) where

import Hedgehog (Group(..), Property, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.AgdaToScalaExpr.Terms (compileBodyTerm, Env(..))
import Agda.Compiler.Scala.ScalaExpr (ScalaTerm(..))
import Agda.Compiler.Scala.AgdaToScalaExpr.Types (CompileError)

import Agda.Syntax.Common (Arg(..), defaultArg)
import Agda.Syntax.Internal (Term(..), Elim'(..))

tests :: Group
tests =
  Group "Terms / application"
    [ ("Def with k Apply args compiles to application of arity k", prop_def_apply_arity)
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
