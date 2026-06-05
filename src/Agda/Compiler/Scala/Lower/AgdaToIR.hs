{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- handle interactions with Agda interlans
module Agda.Compiler.Scala.Lower.AgdaToIR
  ( lowerDefinition
  ) where

import Agda.Compiler.Backend
  ( CompilerPragma
  , Defn(..)
  , RecordData(..)
  , funCompiled
  , dataCons
  , pattern Datatype
  , pattern Function
  )
import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Internal
  ( Dom
  , Tele(..)
  , Telescope
  , Type
  )
import Agda.TypeChecking.CompiledClause (CompiledClauses)
import Agda.TypeChecking.Monad (TCM, getConstInfo)
import Agda.TypeChecking.Monad.Base (Definition(..))
import Agda.TypeChecking.Substitute (absBody)

import Agda.Compiler.Scala.AgdaToScalaExpr.Terms
  ( compileFunctionBody
  )
import Agda.Compiler.Scala.AgdaToScalaExpr.Types
  ( CompileError(..)
  , TyEnv
  , binderName
  , compileDomTypeWith
  , ctorArgTypesFromTypeWith
  , dataTyParamsFromType
  , fromQName
  , funSchemeFromType
  )
import Agda.Compiler.Scala.IR.Agda
  ( AgdaData(..)
  , AgdaDecl(..)
  , AgdaFun(..)
  , AgdaRecord(..)
  )
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaCtor(..)
  , ScalaName
  , SeVar(..)
  )

lowerDefinition :: Definition -> CompilerPragma -> TCM (Either CompileError AgdaDecl)
lowerDefinition def _pragma = case def of
  -- https://hackage-content.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#v:Datatype
  Defn{theDef = Datatype{dataCons = cons}, defName = qn, defType = ty} ->
    lowerData qn ty cons

  Defn{theDef = RecordDefn (RecordData{_recTel = tel}), defName = qn, defType = ty} ->
    pure (DRecord <$> lowerRecord qn ty tel)

  -- https://hackage-content.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#v:Function
  Defn{theDef = Function{funCompiled = cc}, defName = qn, defType = ty} ->
    pure (DFun <$> lowerFun qn ty cc)

  Defn{defName = qn} ->
    pure (Left (UnsupportedDefinition qn))

-- ===== Records ===============================================================

lowerRecord :: QName -> Type -> Telescope -> Either CompileError AgdaRecord
lowerRecord qn recTy tel = do
  (tps, tyEnv) <- dataTyParamsFromType recTy
  fields <- traverse (lowerField tyEnv) (zip [0 :: Int ..] (teleToList tel))
  pure
    AgdaRecord
      { arName = fromQName qn
      , arTyParams = tps
      , arFields = fields
      }

lowerField :: TyEnv -> (Int, Dom Type) -> Either CompileError SeVar
lowerField tyEnv (i, dom) = do
  ty <- compileDomTypeWith tyEnv dom
  pure (SeVar (binderName i dom) ty)

-- Agda.Syntax.Internal.Tele / Telescope:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Tele
--
-- Telescope is linked, not a normal Haskell list.
teleToList :: Telescope -> [Dom Type]
teleToList = \case
  EmptyTel ->
    []
  ExtendTel dom rest ->
    dom : teleToList (absBody rest)

-- ===== Datatypes =============================================================

lowerData :: QName -> Type -> [QName] -> TCM (Either CompileError AgdaDecl)
lowerData typeName typeTy cons = do
  let eParams = dataTyParamsFromType typeTy
  eCtors <- traverse (lowerCtor eParams) cons
  pure $ do
    (tps, _tyEnv) <- eParams
    ctors <- sequence eCtors
    pure
      ( DData
          AgdaData
            { adName = fromQName typeName
            , adTyParams = tps
            , adCtors = ctors
            }
      )

lowerCtor
  :: Either CompileError ([ScalaName], TyEnv)
  -> QName
  -> TCM (Either CompileError ScalaCtor)
lowerCtor eParams conQName = do
  conDef <- getConstInfo conQName
  let conTy = defType conDef
  pure $ do
    (_tps, tyEnv) <- eParams
    argTys <- ctorArgTypesFromTypeWith tyEnv conTy
    pure
      ScalaCtor
        { scName = fromQName conQName
        , scArgs = argTys
        }

-- ===== Functions =============================================================

lowerFun
  :: QName
  -> Type
  -> Maybe CompiledClauses
  -> Either CompileError AgdaFun
lowerFun qn defTy mcc = do
  (args, scheme) <- funSchemeFromType defTy
  body <- compileFunctionBody (argNames args) mcc
  pure
    AgdaFun
      { afName = fromQName qn
      , afArgs = args
      , afScheme = scheme
      , afBody = body
      }
  where
    argNames = map (\(SeVar n _) -> n)
