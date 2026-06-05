{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr.Terms (
    Env (..),
    envFromArgs,
    lookupVar,
    compileFunctionBody,
    compileBodyTerm,
) where


import Data.Maybe (catMaybes)
import Agda.Syntax.Common (Arg (..))
import Agda.Syntax.Internal (ConHead (..), Elim' (..), Term (..))
import Agda.Syntax.Literal (Literal (..))
import Agda.TypeChecking.CompiledClause (CompiledClauses, CompiledClauses' (..))
import qualified Data.Text as T

import Agda.Compiler.Scala.AgdaToScalaExpr.Types (CompileError (..), fromQName)
import Agda.Compiler.Scala.NamePolicy (ctorName, defaultNamePolicy)
import Agda.Compiler.Scala.ScalaExpr (ScalaName, ScalaTerm (..))

-- ===== Term variable environment ============================================

{- | Term-variable environment for resolving de Bruijn Vars in *terms*.
Convention: index 0 is the most recent binder.
-}
newtype Env = Env {unEnv :: [ScalaName]}
    deriving (Eq, Show)

-- Args come in source order; we want env[0] = last binder (de Bruijn 0).
envFromArgs :: [ScalaName] -> Env
envFromArgs = Env . reverse

lookupVar :: Env -> Int -> Either CompileError ScalaName
lookupVar (Env xs) i =
    case drop i xs of
        v : _ -> Right v
        [] -> Left (VarOutOfRange i (length xs))

-- ===== Function bodies =======================================================

compileFunctionBody :: [ScalaName] -> Maybe CompiledClauses -> Either CompileError ScalaTerm
compileFunctionBody _ Nothing = Left UnsupportedCompiledClauses
compileFunctionBody argNs (Just cc) =
    case cc of
        Case{} -> Left UnsupportedCompiledClauses
        Done _ term -> compileBodyTerm (envFromArgs argNs) term
        _ -> Left UnsupportedCompiledClauses

-- ===== Terms ================================================================

compileBodyTerm :: Env -> Term -> Either CompileError ScalaTerm
compileBodyTerm env = \case
    Var i elims -> do
        f <- STeVar <$> lookupVar env i
        applyElims env f elims
    Def qn elims -> do
        let f = STeVar (fromQName qn)
        applyElims env f elims
    Con ch _ es -> compileConApp env ch es
    Lit lit -> compileLiteral lit
    t -> Left (UnsupportedTerm t)

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
compileElimMaybe env = \case
    Apply a ->
        case unArg a of
            -- erase universe-level artifacts at term level
            Level _ -> pure Nothing
            Sort _  -> pure Nothing
            t       -> Just <$> compileBodyTerm env t
    _ -> Left (UnsupportedTerm (Var 0 [])) -- Proj/IApply later

compileLiteral :: Literal -> Either CompileError ScalaTerm
compileLiteral = \case
    LitNat n -> pure (STeLitInt (fromIntegral n))
    LitWord64 n -> pure (STeLitInt (fromIntegral n))
    LitString s -> pure (STeLitString (T.unpack s))
    l -> Left (UnsupportedTerm (Lit l))
