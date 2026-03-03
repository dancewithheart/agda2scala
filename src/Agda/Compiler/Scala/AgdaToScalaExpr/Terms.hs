{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr.Terms
  ( Env(..)
  , envFromArgs
  , lookupVar
  , compileFunctionBody
  , compileBodyTerm
  ) where

import qualified Data.Text as T

import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Internal (ConHead(..), Elim'(..), Term(..))
import Agda.Syntax.Common (Arg(..))
import Agda.Syntax.Literal (Literal(..))
import Agda.TypeChecking.CompiledClause (Case, CompiledClauses(..), CompiledClauses'(..))

import Agda.Compiler.Scala.ScalaExpr (ScalaName, ScalaTerm(..))
import Agda.Compiler.Scala.AgdaToScalaExpr.Types (CompileError(..), fromQName)

-- ===== Term variable environment ============================================

-- | Term-variable environment for resolving de Bruijn Vars in *terms*.
-- Convention: index 0 is the most recent binder.
newtype Env = Env { unEnv :: [ScalaName] }
  deriving (Eq, Show)

-- Args come in source order; we want env[0] = last binder (de Bruijn 0).
envFromArgs :: [ScalaName] -> Env
envFromArgs = Env . reverse

lookupVar :: Env -> Int -> Either CompileError ScalaName
lookupVar (Env xs) i =
  case drop i xs of
    v:_ -> Right v
    []  -> Left (VarOutOfRange i (length xs))

-- ===== Function bodies =======================================================

compileFunctionBody :: [ScalaName] -> Maybe CompiledClauses -> Either CompileError ScalaTerm
compileFunctionBody _ Nothing   = Left UnsupportedCompiledClauses
compileFunctionBody argNs (Just cc) =
  case cc of
    Case{}      -> Left UnsupportedCompiledClauses
    Done _ term -> compileBodyTerm (envFromArgs argNs) term
    _           -> Left UnsupportedCompiledClauses

-- ===== Terms ================================================================

compileBodyTerm :: Env -> Term -> Either CompileError ScalaTerm
compileBodyTerm env = \case
  Var i _      -> STeVar <$> lookupVar env i
  Def qn _     -> pure (STeVar (fromQName qn))
  Con ch _ es  -> compileConApp env ch es
  Lit lit      -> compileLiteral lit
  t            -> Left (UnsupportedTerm t)

compileConApp :: Env -> ConHead -> [Elim' Term] -> Either CompileError ScalaTerm
compileConApp env conHead elims = do
  args <- traverse (compileElim env) elims
  let f = STeVar (fromQName (conName conHead))
  pure $ case args of
    [] -> f
    _  -> STeApp f args

compileElim :: Env -> Elim' Term -> Either CompileError ScalaTerm
compileElim env = \case
  Apply a -> compileBodyTerm env (unArg a)
  _       -> Left (UnsupportedTerm (Var 0 [])) -- refine later (Proj/IApply)

compileLiteral :: Literal -> Either CompileError ScalaTerm
compileLiteral = \case
  LitNat n    -> pure (STeLitInt (fromIntegral n))
  LitWord64 n -> pure (STeLitInt (fromIntegral n))
  LitString s -> pure (STeLitString (T.unpack s))
  l           -> Left (UnsupportedTerm (Lit l))
