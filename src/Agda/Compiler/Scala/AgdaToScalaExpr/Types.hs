{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr.Types (
    CompileError (..),
    TyEnv (..),
    emptyTyEnv,
    lookupTyVar,
    unrollPi,
    isTypeParamBinder,
    ctorArgTypesFromTypeWith,
    ctorArgTypesFromType,
    dataTyParamsFromType,
    funSchemeFromType,
    compileDomTypeWith,
    compileTypeWith,
    compileType,
    compileTypeTermWith,
    compileTypeTerm,
    binderName,
    fromQName,
    pushTermBinder,
    pushTyParam,
) where

import Control.Monad (foldM)

import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Common (Arg (..), Hiding (..), NamedName, Ranged (..), WithOrigin (..), getHiding)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Internal (
    Abs,
    ConHead (..),
    Dom,
    Dom' (..),
    Elim' (..),
    Term (..),
    Type,
    Type'' (..),
    qnameName,
    unDom,
 )
import Agda.TypeChecking.Substitute (absBody)

import Agda.Compiler.Scala.NameEnv (sanitizeScalaIdent)
import Agda.Compiler.Scala.ScalaExpr (
    ScalaName,
    ScalaType (..),
    ScalaTypeScheme (..),
    SeVar (..),
 )

-- ===== Errors ================================================================

data CompileError
    = UnsupportedDefinition QName
    | UnsupportedType Type
    | UnsupportedTerm Term
    | UnsupportedCompiledClauses
    | VarOutOfRange Int Int
    deriving (Eq, Show)

-- ===== Type variable environment ============================================

{- | TyEnv is aligned with the full Π telescope of a 'Type'.

Agda uses de Bruijn indices across *all* binders (type params and term params).
For example:

  id : {A : Type u} -> A -> A

The result 'A' is often Var 1 (because the term binder sits at index 0).
To resolve Vars correctly, TyEnv stores:
  Just "A"  for type parameters we want to surface in Scala
  Nothing   for term binders (to keep indices aligned)

Index 0 is the most recent binder.
-}
newtype TyEnv = TyEnv {unTyEnv :: [Maybe ScalaName]}
    deriving (Eq, Show)

emptyTyEnv :: TyEnv
emptyTyEnv = TyEnv []

pushTyParam :: ScalaName -> TyEnv -> TyEnv
pushTyParam a (TyEnv xs) = TyEnv (Just a : xs)

pushTermBinder :: TyEnv -> TyEnv
pushTermBinder (TyEnv xs) = TyEnv (Nothing : xs)

lookupTyVar :: TyEnv -> Int -> ScalaName
lookupTyVar (TyEnv xs) i =
    case drop i xs of
        (Just v : _) -> v
        (Nothing : _) -> "t" <> show i -- TODO dependent term binder; fallback for now
        [] -> "t" <> show i

-- ===== Pi traversal ==========================================================

-- | Split a type into Pi binders and a result type. Stops at first non-Pi.
unrollPi :: Type -> Either CompileError ([(Dom Type, Abs Type)], Type)
unrollPi = go []
  where
    go acc = \case
        El _ (Pi dom absTy) ->
            go ((dom, absTy) : acc) (absBody absTy)
        ty ->
            pure (reverse acc, ty)

-- ===== Type compilation ======================================================

compileDomTypeWith :: TyEnv -> Dom Type -> Either CompileError ScalaType
compileDomTypeWith tyEnv = compileTypeWith tyEnv . unDom

compileTypeWith :: TyEnv -> Type -> Either CompileError ScalaType
compileTypeWith tyEnv = \case
    El _ t -> compileTypeTermWith tyEnv t
    t -> Left (UnsupportedType t)

-- Agda.Syntax.Internal.Type:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Type
compileType :: Type -> Either CompileError ScalaType
compileType = compileTypeWith emptyTyEnv

-- Agda represents type application via eliminations (elims) on Def/Var.
-- Example: List A appears as Def List [Apply A].
-- We translate Apply args into STyApp/List[A].
compileTypeTermWith :: TyEnv -> Term -> Either CompileError ScalaType
compileTypeTermWith tyEnv = \case
    Def qn elims -> do
        args <- compileTypeArgs tyEnv elims
        let f0 = fromQName qn
            f = mapBuiltinTypeName f0
        pure $ case args of
            [] -> STyName f
            _ -> STyApp f args
    Var n elims -> do
        args <- compileTypeArgs tyEnv elims
        let f = lookupTyVar tyEnv n
        pure $ case args of
            [] -> STyVar f
            _ -> STyApp f args
    Con c _ _ -> Right (STyName (fromQName (conName c)))
    Sort _ -> Right (STyName "Type")
    t -> Left (UnsupportedTerm t)

{- | Extract type parameters from a data/record type signature.
We treat only binders whose dom type is type-like (Sort/Pi..Sort) as type params.
Example: Maybe (A : Type u) : Type u   -- A is NotHidden but type-like
-}
dataTyParamsFromType :: Type -> Either CompileError ([ScalaName], TyEnv)
dataTyParamsFromType ty0 = do
    (pis, _res) <- unrollPi ty0
    -- fold like funSchemeFromType but without term args; for datatypes
    foldM step ([], emptyTyEnv) (zip [0 :: Int ..] pis)
  where
    step (ps, env) (i, (dom, _absTy)) =
        if isDataTypeParamBinder dom
            then
                let a = binderName i dom
                 in pure (ps <> [a], pushTyParam a env)
            else
                -- datatype indices/term params are possible; keep env aligned
                pure (ps, pushTermBinder env)

compileTypeArgs :: TyEnv -> [Elim' Term] -> Either CompileError [ScalaType]
compileTypeArgs tyEnv elims =
    traverse fromApply [a | Apply a <- elims]
  where
    fromApply :: Arg Term -> Either CompileError ScalaType
    fromApply a = compileTypeTermWith tyEnv (unArg a)

-- Agda.Syntax.Internal.Term:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Term
-- Backwards-compatible export: empty type-var env.
compileTypeTerm :: Term -> Either CompileError ScalaType
compileTypeTerm = compileTypeTermWith emptyTyEnv

-- Constructor types are Π-chains. We fold Π binders left-to-right.
-- Later argument types may reference earlier binders.
-- We must pushTermBinder after each explicit argument to keep the TyEnv aligned.
-- This fixes cases like:
--   Cons : A -> List A -> List A
-- where the second arg 'List A' refers to A with a shifted de Bruijn index.
ctorArgTypesFromTypeWith :: TyEnv -> Type -> Either CompileError [ScalaType]
ctorArgTypesFromTypeWith env0 ty0 = do
    (pis, _res) <- unrollPi ty0
    (argTysRev, _envFinal) <- foldM step ([], env0) (zip [0 :: Int ..] pis)
    pure (reverse argTysRev)
  where
    step (acc, env) (i, (dom, _absTy)) =
        case getHiding dom of
            Hidden ->
                -- ctor-level implicit binders: keep TyEnv aligned, but don't emit args
                if isDataTypeParamBinder dom
                    then
                        let a = binderName i dom
                         in pure (acc, pushTyParam a env)
                    else
                        pure (acc, pushTermBinder env)
            _ -> do
                ty <- compileDomTypeWith env dom
                pure (ty : acc, pushTermBinder env)

ctorArgTypesFromType :: Type -> Either CompileError [ScalaType]
ctorArgTypesFromType = ctorArgTypesFromTypeWith emptyTyEnv

-- ===== Type scheme extraction ===============================================

{- | Classification of Π-binders into Scala type parameters vs term parameters.

Agda represents both implicit and explicit binders as Π (Pi) in 'defType'.
A binder's *hiding* (Hidden/NotHidden) is not enough to decide whether it is a
type parameter:

  • Datatypes/records commonly have *explicit* type parameters:
      data Maybe (A : Type u) : Type u
    so we should treat type-like binders as type params regardless of hiding.

  • Functions often have implicit type parameters and explicit term parameters:
      id : {A : Type u} -> A -> A
    Here we usually want only *Hidden + type-like* binders as Scala type params,
    to avoid turning explicit term arguments into type params.

The helpers below encode this policy in one place.
-}
isTypeParamBinder :: Dom Type -> Bool
isTypeParamBinder dom = getHiding dom == Hidden && isTypeLike (unDom dom)

-- | Datatypes/records: treat any type-like binder as a Scala type parameter.
isDataTypeParamBinder :: Dom Type -> Bool
isDataTypeParamBinder dom = isTypeLike (unDom dom)

{- | Functions: default policy is "Hidden + type-like" is a Scala type parameter.
(We still track other binders in TyEnv to keep de Bruijn indices aligned.)
-}
isFunTypeParamBinder :: Dom Type -> Bool
isFunTypeParamBinder dom = getHiding dom == Hidden && isTypeLike (unDom dom)

{- | True if the binder's type is a universe (Type/Set) or a type constructor ending in a universe.
This lets us distinguish implicit *type parameters* from implicit *term arguments*.

In Agda internal syntax, "A : Type u" appears as El _ (Sort _).
A higher-kinded parameter like "F : Type u -> Type u" appears as El _ (Pi _ -> Sort _).
-}
isTypeLike :: Type -> Bool
isTypeLike = \case
    El _ t -> goTerm t
    _ -> False
  where
    goTerm = \case
        Sort _ -> True
        Pi _ absTy -> isTypeLike (absBody absTy)
        _ -> False

{- | Compute value arguments and polymorphic scheme from a function type.
Hidden Pis become `ssTyParams`; explicit Pis become `[SeVar]`.
-}
funSchemeFromType :: Type -> Either CompileError ([SeVar], ScalaTypeScheme)
funSchemeFromType ty0 = do
    (pis, resTy) <- unrollPi ty0
    (args, tps, env) <- foldM step ([], [], emptyTyEnv) (zip [0 :: Int ..] pis)
    ret <- compileTypeWith env resTy
    pure (reverse args, ScalaTypeScheme (reverse tps) ret)
  where
    step (argsAcc, tpsAcc, env) (i, (dom, _)) =
        case getHiding dom of
            Hidden
                | isFunTypeParamBinder dom ->
                    let a = binderName i dom
                     in pure (argsAcc, a : tpsAcc, pushTyParam a env)
            -- Any other binder is a term binder (explicit or implicit):
            _ -> do
                ty <- compileDomTypeWith env dom
                let x = binderName i dom
                pure (SeVar x ty : argsAcc, tpsAcc, pushTermBinder env)

-- ===== Naming ================================================================

-- Binder names should never be empty.
binderName :: Int -> Dom Type -> ScalaName
binderName i dom =
    case domName dom of
        Just a ->
            let s = sanitizeScalaIdent (namedNameToStr a)
             in if null s then ("x" <> show i) else s
        Nothing -> "x" <> show i

namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

-- QName -> ScalaName
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Abstract-Name.html#t:QName
fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName

mapBuiltinTypeName :: ScalaName -> ScalaName
mapBuiltinTypeName n = case n of
    "Nat" -> "Long"
    "ℕ" -> "Long"
    "Bool" -> "Boolean"
    -- TODO String?
    _ -> n
