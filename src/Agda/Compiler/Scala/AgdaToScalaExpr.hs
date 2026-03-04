{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr
  ( compileDefn
  , CompileError(..)
  , compileTypeTerm
  , compileBodyTerm
  , lookupVar
  , Env(..)
  ) where

import Control.Monad (foldM)

import Agda.Compiler.Backend (CompilerPragma, Defn(..), RecordData(..), funCompiled)
import Agda.Compiler.Backend --TODO explicitly list dependencies
import Agda.Syntax.Abstract.Name ()
import Agda.Syntax.Common (Hiding(..), getHiding)
import Agda.Syntax.Internal (Abs, Dom(..), Tele(..), Telescope, Type, Type''(..))
import Agda.TypeChecking.Monad (TCM, getConstInfo)
import Agda.TypeChecking.Monad.Base (Definition(..))
import Agda.TypeChecking.CompiledClause (CompiledClauses)
import Agda.TypeChecking.Substitute (absBody)

import Agda.Compiler.Scala.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr(..)
  , ScalaName
  , ScalaType(..)
  , SeVar(..)
  )

import Agda.Compiler.Scala.AgdaToScalaExpr.Types
  ( CompileError(..)
  , TyEnv(..)
  , ctorArgTypesFromTypeWith
  , ctorArgTypesFromType
  , dataTyParamsFromType
  , emptyTyEnv
  , unrollPi
  , funSchemeFromType
  , compileDomTypeWith
  , compileTypeTerm
  , binderName
  , fromQName
  , pushTermBinder
  , pushTyParam
  )

import Agda.Compiler.Scala.AgdaToScalaExpr.Terms
  ( Env(..)
  , lookupVar
  , compileBodyTerm
  , compileFunctionBody
  )

-- ===== Entry point ===========================================================

-- | Compile an Agda 'Definition' to Scala AST or return a structured error.
-- Agda.TypeChecking.Monad.Base.Definition:
-- https://hackage.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#t:Definition
compileDefn :: Definition -> CompilerPragma -> TCM (Either CompileError ScalaExpr)
compileDefn def _pragma = compileDefinition def

compileDefinition :: Definition -> TCM (Either CompileError ScalaExpr)
compileDefinition = \case
  Defn{theDef = Datatype{dataCons = cons}, defName = qn, defType = ty} ->
    compileDataType qn ty cons

  Defn{theDef = Function{funCompiled = cc}, defName = qn, defType = ty} ->
    pure (compileFunction qn ty cc)

  Defn{theDef = RecordDefn (RecordData{_recTel = tel}), defName = qn, defType = ty} ->
    pure (compileRecord qn ty tel)

  Defn{defName = qn} ->
    pure (Left (UnsupportedDefinition qn))

-- ===== Records ===============================================================

-- | Compile a record telescope into a Scala case class.
-- Agda.Syntax.Internal.Telescope (list-like):
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Telescope
compileRecord :: QName -> Type -> Telescope -> Either CompileError ScalaExpr
compileRecord qn recTy tel = do
  (tps, tyEnv) <- dataTyParamsFromType recTy
  vars <- traverse (compileField tyEnv) (zip [0 :: Int ..] (teleToList tel))
  pure (SeProd (fromQName qn) tps vars)
  where
    compileField :: TyEnv -> (Int, Dom Type) -> Either CompileError SeVar
    compileField tyEnv (i, dom) = do
      ty <- compileDomTypeWith tyEnv dom
      pure (SeVar (binderName i dom) ty)

-- Agda.Syntax.Internal.Tele / Telescope:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Tele
-- Telescope is a linked structure, not a list.
teleToList :: Telescope -> [Dom Type]
teleToList = \case
  EmptyTel        -> []
  ExtendTel dom t -> dom : teleToList (absBody t)

-- ===== Datatypes / constructors =============================================

compileDataType :: QName -> Type -> [QName] -> TCM (Either CompileError ScalaExpr)
compileDataType typeName typeTy cons = do
  let eParams = dataTyParamsFromType typeTy
  eCtors <- traverse (compileCtorWith eParams) cons
  pure $ do
    (tps, _tyEnv) <- eParams
    ctors <- sequence eCtors
    pure (SeSum (fromQName typeName) tps ctors)

-- Compile constructors using the datatype TyEnv
compileCtorWith
  :: Either CompileError ([ScalaName], TyEnv)
  -> QName
  -> TCM (Either CompileError ScalaCtor)
compileCtorWith eParams conQName = do
  conDef <- getConstInfo conQName
  let conTy = defType conDef
  pure $ do
    (_tps, tyEnv) <- eParams
    argTys <- ctorArgTypesFromTypeWith tyEnv conTy
    pure ScalaCtor { scName = fromQName conQName, scArgs = argTys }

compileCtor :: QName -> TCM (Either CompileError ScalaCtor)
compileCtor conQName = do
  conDef <- getConstInfo conQName
  let conTy = defType conDef
  pure $ do
    argTys <- ctorArgTypesFromType conTy
    pure ScalaCtor { scName = fromQName conQName, scArgs = argTys }

-- ===== Functions =============================================================

compileFunction
  :: QName
  -> Type
  -> Maybe CompiledClauses
  -> Either CompileError ScalaExpr
compileFunction qn defTy mcc = do
  (args, scheme) <- funSchemeFromType defTy
  body <- compileFunctionBody (argNames args) mcc
  pure (SeFun (fromQName qn) args scheme body)
    where
      argNames = map (\(SeVar n _) -> n)
