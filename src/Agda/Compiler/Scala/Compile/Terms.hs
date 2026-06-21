{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.Compile.Terms (
    Env (..),
    envFromArgs,
    extendEnv,
    lookupVar,
    compileFunctionBody,
    compileBodyTerm,
) where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Map as Map
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common
  ( Arg(..)
  , Hiding(..)
  , getHiding
  )
import Agda.Syntax.Internal (ConHead (..), Elim' (..), Term (..))
import Agda.Syntax.Literal (Literal (..))
import Agda.TypeChecking.CompiledClause
  ( Case(..)
  , CompiledClauses
  , CompiledClauses'(..)
  , WithArity(..)
  )
import qualified Data.Text as T

import Agda.Compiler.Scala.Compile.Types
  ( CompileError (..)
  , CaseUnsupported (..)
  , fromQName)
import Agda.Compiler.Scala.Name.NamePolicy (ctorName, defaultNamePolicy, termName)
import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaName
  , ScalaPat(..)
  , ScalaTerm(..)
  )

-- ===== Environment ===========================================================
--
-- Agda terms use de Bruijn indices:
--
--   Var 0 = most recently introduced binder
--   Var 1 = binder before that
--
-- We keep that convention directly in Env:
--
--   Env ["last", "first"]
--
-- Therefore all places that introduce binders must go through envFromArgs
-- or extendEnv.

newtype Env = Env {unEnv :: [ScalaName]}
    deriving (Eq, Show)

-- Args come in source order; we want env[0] = last binder (de Bruijn 0).
envFromArgs :: [ScalaName] -> Env
envFromArgs names = Env (reverse names)

freshPatVars :: Int -> [ScalaName]
freshPatVars arityN = [ "p" <> show i | i <- [0 .. arityN - 1] ]

-- Agda de Bruijn convention used here:
-- Env index 0 is the most recently introduced binder.
--
-- Constructor arguments are printed left-to-right:
--   C(p0, p1)
--
-- But de Bruijn lookup sees the newest binder first:
--   Var 0 -> p1
--   Var 1 -> p0
extendEnv :: [ScalaName] -> Env -> Env
extendEnv names (Env xs) = Env (reverse names <> xs)

lookupVar :: Env -> Int -> Either CompileError ScalaName
lookupVar (Env xs) i = case drop i xs of
  v : _ -> Right v
  []    -> Left (VarOutOfRange i (length xs))

-- ===== Function bodies =======================================================

-- Function bodies are read from `CompiledClauses`, not from surface syntax.
-- Pattern matching therefore appears as Agda's compiled case tree.
compileFunctionBody :: [ScalaName] -> Maybe CompiledClauses -> Either CompileError ScalaTerm
compileFunctionBody _ Nothing       = Left UnsupportedCompiledClauses
compileFunctionBody argNs (Just cc) = compileCompiledClauses (envFromArgs argNs) cc

compileCompiledClauses :: Env -> CompiledClauses -> Either CompileError ScalaTerm
compileCompiledClauses env = \case
    Done _ term -> compileBodyTerm env term
    Case arg branches -> do
      scrut <- STeVar <$> lookupVar env (unArg arg)
      alts <- compileBranches env branches
      pure (STeMatch scrut alts)
    _ -> Left UnsupportedCompiledClauses

compileBranches :: Env -> Case CompiledClauses -> Either CompileError [(ScalaPat, ScalaTerm)]
compileBranches env branches = do
    validateCaseShape branches
    traverse compileConBranch (Map.toList (conBranches branches))
  where
    compileConBranch (conQName, WithArity arityN cc) = do
      let patVars = freshPatVars arityN
          pat     = SPCtor (fromQName conQName) (map SPVar patVars)
          env'    = extendEnv patVars env
      rhs <- compileCompiledClauses env' cc
      pure (pat, rhs)

-- Reject unsupported case-tree shapes explicitly.
-- Silent branch dropping would generate partial Scala matches.
validateCaseShape :: Case CompiledClauses -> Either CompileError ()
validateCaseShape branches
    | projPatterns branches                  = Left $ UnsupportedCaseShape HasProjectionPatterns
    | not (Map.null (litBranches branches))  = Left $ UnsupportedCaseShape HasLiteralBranches
    | isJust (catchallBranch branches)       = Left $ UnsupportedCaseShape HasCatchAllBranch
    | fromMaybe False (fallThrough branches) = Left $ UnsupportedCaseShape HasFallThrough
    | otherwise                              = Right ()

-- ===== Terms ================================================================

compileBodyTerm :: Env -> Term -> Either CompileError ScalaTerm
compileBodyTerm env = \case
    Var i elims -> do
        f <- STeVar <$> lookupVar env i
        applyElims env f elims
    Def qn elims
      | fromQName qn == "if_then_else_" ->compileIfThenElse env qn elims
      | fromQName qn == "_<ᵇ_" -> compileBinaryOp env qn "<" elims
      | fromQName qn == "_<_" -> compileBinaryOp env qn "<" elims
      | otherwise -> do
          let f = STeVar (termName defaultNamePolicy (fromQName qn))
          applyElims env f elims
    Con ch _ es -> compileConApp env ch es
    Lit lit -> compileLiteral lit
    t -> Left (UnsupportedTerm t)

compileIfThenElse :: Env -> QName -> [Elim' Term] -> Either CompileError ScalaTerm
compileIfThenElse env qn elims =
  case visibleApplyTerms elims of
    [cond, thenBranch, elseBranch] ->
      STeIf
        <$> compileBodyTerm env cond
        <*> compileBodyTerm env thenBranch
        <*> compileBodyTerm env elseBranch
    _ -> Left (UnsupportedTerm (Def qn elims))

compileBinaryOp :: Env -> QName -> ScalaName -> [Elim' Term] -> Either CompileError ScalaTerm
compileBinaryOp env qn op elims =
  case visibleApplyTerms elims of
    [lhs, rhs] ->
      STeBinOp
        <$> compileBodyTerm env lhs
        <*> pure op
        <*> compileBodyTerm env rhs
    _ -> Left (UnsupportedTerm (Def qn elims))

visibleApplyTerms :: [Elim' Term] -> [Term]
visibleApplyTerms elims =
  [ unArg arg
  | Apply arg <- elims
  , getHiding arg == NotHidden
  ]

compileConApp :: Env -> ConHead -> [Elim' Term] -> Either CompileError ScalaTerm
compileConApp env conHead elims = do
    args <- fmap catMaybes (traverse (compileElimMaybe env) elims)
    let f = STeVar (ctorName defaultNamePolicy (fromQName (conName conHead)))
    pure $ case args of
        [] -> f
        _  -> STeApp f args

applyElims :: Env -> ScalaTerm -> [Elim' Term] -> Either CompileError ScalaTerm
applyElims env f elims = do
  args <- fmap catMaybes (traverse (compileElimMaybe env) elims)
  pure $ case args of
    [] -> f
    _  -> STeApp f args

compileElimMaybe :: Env -> Elim' Term -> Either CompileError (Maybe ScalaTerm)
compileElimMaybe env elim = case elim of
    Apply arg
      | getHiding arg /= NotHidden -> Right Nothing
      | isErasedTypeArgument (unArg arg) -> Right Nothing
      | otherwise -> Just <$> compileBodyTerm env (unArg arg)
    _ -> Right Nothing

isErasedTypeArgument :: Term -> Bool
isErasedTypeArgument term = case term of
  Level{} -> True
  Sort{}  -> True
  _       -> False

compileLiteral :: Literal -> Either CompileError ScalaTerm
compileLiteral = \case
    LitNat n -> pure (STeLitInt (fromIntegral n))
    LitWord64 n -> pure (STeLitInt (fromIntegral n))
    LitString s -> pure (STeLitString (T.unpack s))
    l -> Left (UnsupportedTerm (Lit l))
