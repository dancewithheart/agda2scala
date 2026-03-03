{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr.Types
  ( CompileError(..)
  , TyEnv(..)
  , emptyTyEnv
  , extendTyEnv
  , lookupTyVar
  , unrollPi
  , isTypeParamBinder
  , collectTypeParams
  , funSchemeFromType
  , compileDomTypeWith
  , compileTypeWith
  , compileType
  , compileTypeTermWith
  , compileTypeTerm
  , binderName
  , fromQName
  ) where

import qualified Data.Text as T

import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Common (Hiding(..), getHiding, NamedName, WithOrigin(..), Ranged(..))
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Internal
  ( Abs
  , ConHead(..)
  , Dom(..)
  , Dom'(..)
  , Term(..)
  , Type
  , Type''(..)
  , qnameName
  , unDom
  )
import Agda.TypeChecking.Substitute (absBody)

import Agda.Compiler.Scala.NameEnv (sanitizeScalaIdent)
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaName
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , SeVar(..)
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

-- | Type-variable environment for resolving de Bruijn Vars in *types*.
-- Convention: index 0 is the most recently-bound type variable.
newtype TyEnv = TyEnv { unTyEnv :: [ScalaName] }
  deriving (Eq, Show)

emptyTyEnv :: TyEnv
emptyTyEnv = TyEnv []

extendTyEnv :: ScalaName -> TyEnv -> TyEnv
extendTyEnv a (TyEnv xs) = TyEnv (a : xs)

lookupTyVar :: TyEnv -> Int -> ScalaName
lookupTyVar (TyEnv xs) i =
  case drop i xs of
    v:_ -> v
    []  -> "t" <> show i

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
  t      -> Left (UnsupportedType t)

-- Agda.Syntax.Internal.Type:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Type
compileType :: Type -> Either CompileError ScalaType
compileType = compileTypeWith emptyTyEnv

compileTypeTermWith :: TyEnv -> Term -> Either CompileError ScalaType
compileTypeTermWith tyEnv = \case
  Def qn _  -> Right (STyName (fromQName qn))
  Var n _   -> Right (STyVar (lookupTyVar tyEnv n))
  Con c _ _ -> Right (STyName (fromQName (conName c)))
  Sort _    -> Right (STyName "Type")
  t         -> Left (UnsupportedTerm t)

-- Agda.Syntax.Internal.Term:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Term
-- Backwards-compatible export: empty type-var env.
compileTypeTerm :: Term -> Either CompileError ScalaType
compileTypeTerm = compileTypeTermWith emptyTyEnv

-- ===== Type scheme extraction ===============================================

-- | Heuristic: treat all Hidden Pi binders as type parameters for now.
-- Later refine (e.g. inspect dom type to distinguish implicit term args).
isTypeParamBinder :: Dom Type -> Bool
isTypeParamBinder dom = getHiding dom == Hidden

collectTypeParams :: [(Dom Type, Abs Type)] -> ([ScalaName], TyEnv)
collectTypeParams pis =
  foldl step ([], emptyTyEnv) (zip [0 :: Int ..] pis)
  where
    step (ps, env) (i, (dom, _abs)) =
      if isTypeParamBinder dom
        then
          let nm = binderName i dom
          in (ps <> [nm], extendTyEnv nm env)
        else (ps, env)

-- | Compute value arguments and polymorphic scheme from a function type.
-- Hidden Pis become `ssTyParams`; explicit Pis become `[SeVar]`.
funSchemeFromType :: Type -> Either CompileError ([SeVar], ScalaTypeScheme)
funSchemeFromType ty0 = do
  (pis, resTy) <- unrollPi ty0
  let (tyParams, tyEnv) = collectTypeParams pis

  args <- fmap concat $ traverse (mkArg tyEnv) (zip [0 :: Int ..] pis)
  ret  <- compileTypeWith tyEnv resTy

  pure (args, ScalaTypeScheme { ssTyParams = tyParams, ssType = ret })
  where
    mkArg tyEnv (i, (dom, _absTy)) =
      case getHiding dom of
        Hidden -> pure []  -- treated as type params
        _      -> do
          ty <- compileDomTypeWith tyEnv dom
          pure [SeVar (binderName i dom) ty]

-- ===== Naming ================================================================

-- Binder names should never be empty.
binderName :: Int -> Dom Type -> ScalaName
binderName i dom =
  case domName dom of
    Just a  ->
      let s = sanitizeScalaIdent (namedNameToStr a)
      in if null s then ("x" <> show i) else s
    Nothing -> "x" <> show i

namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

-- QName -> ScalaName
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Abstract-Name.html#t:QName
fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
