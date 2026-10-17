{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- Handle interactions with Agda internals.
--
-- Agda backend boundary.
--
-- At this point Agda has already parsed, scope-checked, and type-checked
-- the source module. We inspect checked `Definition`s and lower only the
-- subset currently supported by agda2scala.

module Agda.Compiler.Scala.Lower.AgdaToIR
  ( toAgdaExpr
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
  , unDom
  )
import Agda.TypeChecking.CompiledClause (CompiledClauses)
import Agda.TypeChecking.Monad (TCM, getConstInfo)
import Agda.TypeChecking.Monad.Base (Definition(..))
import Agda.TypeChecking.Substitute (absBody)
import Control.Monad (foldM)

import Agda.Compiler.Scala.Compile.Terms (compileFunctionBody)
import Agda.Compiler.Scala.Compile.Types
  ( CompileError(..)
  , TyEnv
  , compileDomTypeWith
  , ctorArgTypesFromTypeWith
  , dataTyParamsFromType
  , fromQName
  , funSchemeFromType
  , pushTermBinder
  )
import Agda.Compiler.Scala.IR.AgdaIR
  ( AgdaData(..)
  , AgdaDecl(..)
  , AgdaFun(..)
  , AgdaRecord(..)
  )
import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor(..)
  , ScalaName
  , ScalaTypeScheme(..)
  , SeVar(..)
  )

-- `Definition` is Agda's checked top-level definition
toAgdaExpr :: Definition -> CompilerPragma -> TCM (Either CompileError AgdaDecl)
toAgdaExpr def _pragma = case def of
  -- https://hackage-content.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#v:Datatype
  Defn{theDef = Datatype{dataCons = cons}, defName = qn, defType = ty} ->
    lowerData qn ty cons

  Defn{theDef = RecordDefn recordData, defName = qn, defType = ty} ->
    pure (DRecord <$> lowerRecord qn ty recordData)

  -- https://hackage-content.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#v:Function
  Defn{theDef = Function{funCompiled = cc}, defName = qn, defType = ty} ->
    pure (DFun <$> lowerFun qn ty cc)

  Defn{defName = qn} ->
    pure (Left (UnsupportedDefinition qn))

-- ===== Records ===============================================================

lowerRecord :: QName
  -> Type
  -> RecordData
  -> Either CompileError AgdaRecord
lowerRecord qn recTy recordData = do
  (tps, tyEnv0) <- dataTyParamsFromType recTy

  let allDoms = teleToList (_recTel recordData)
      fieldNames = map (fromQName . unDom) (_recFields recordData)
      parameterCount = length allDoms - length fieldNames

  if parameterCount < 0
    then Left (UnsupportedDefinition qn)
    else do
      let fieldDoms = drop parameterCount allDoms

      (fieldsRev, _) <-
        foldM
          lowerRecordField
          ([], tyEnv0)
          (zip fieldNames fieldDoms)

      pure AgdaRecord
        { arName = fromQName qn
        , arTyParams = tps
        , arFields = reverse fieldsRev
        }

lowerRecordField :: ([SeVar], TyEnv)
  -> (ScalaName, Dom Type)
  -> Either CompileError ([SeVar], TyEnv)
lowerRecordField (fields, tyEnv) (name, dom) = do
  ty <- compileDomTypeWith tyEnv dom
  pure
    ( SeVar name ty : fields
    , pushTermBinder tyEnv
    )

-- Agda.Syntax.Internal.Tele / Telescope:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Tele
--
-- Telescopes are linked through `ExtendTel`, not a normal Haskell list.
teleToList :: Telescope -> [Dom Type]
teleToList = \case
  EmptyTel           -> []
  ExtendTel dom rest -> dom : teleToList (absBody rest)

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
  body <- compileFunctionBody (ssTyParams scheme) (argNames args) mcc
  pure
    AgdaFun
      { afName = fromQName qn
      , afArgs = args
      , afScheme = scheme
      , afBody = body
      }
  where
    argNames = map (\(SeVar n _) -> n)
