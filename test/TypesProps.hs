module TypesProps (tests) where

import Hedgehog (Group(..), Gen(..), Property, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.AgdaToScalaExpr.Types
  ( TyEnv(..)
  , compileTypeTermWith
  , lookupTyVar
  )

import Agda.Compiler.Scala.ScalaExpr (ScalaType(..))

import Agda.Syntax.Internal (Term(..), Elim'(..))
import Agda.Syntax.Common (Arg(..))

tests :: Group
tests =
  Group "TypesProps"
    [ ("prop_lookupTyVar_resolves_named", prop_lookupTyVar_resolves_named)
    , ("prop_compileTypeTermWith_varApp_arity", prop_compileTypeTermWith_varApp_arity)
    , ("prop_compileTypeTermWith_shifted_index", prop_compileTypeTermWith_shifted_index)
    ]

-- | TyEnv lookup sanity: resolve named type variables at the right de Bruijn index.
--
-- In Agda internal syntax, type variables are de Bruijn indexed (Var 0 = most recent binder).
-- Our TyEnv mirrors the *full* binder stack:
--   Just "A"  = a type parameter we want to surface in Scala
--   Nothing   = a term binder (kept only to keep indices aligned)
--
-- This property pins two essential facts:
--   1) If the most recent binder is a type param, index 0 resolves to that name.
--   2) If we have one intervening term binder, the same type param shifts to index 1.
--
-- Regression prevented: returning "tN" for a type param that should resolve to "A"
-- (common when TyEnv doesn't include term binders).
prop_lookupTyVar_resolves_named :: Property
prop_lookupTyVar_resolves_named = property $ do
  -- env[0] is most recent binder
  lookupTyVar (TyEnv [Just "A"]) 0 === "A"
  lookupTyVar (TyEnv [Nothing, Just "A"]) 1 === "A"

-- | Type application arity is preserved when compiling a Var with Apply-eliminations.
--
-- Agda encodes type application via eliminations:
--   Var n [Apply a1, Apply a2, ...]
-- represents applying a type variable to arguments (e.g. F A B).
--
-- Our compiler should translate this to:
--   STyVar name              when there are 0 Apply args
--   STyApp name [t1, t2, ..] when there are k Apply args
--
-- This property checks the *arity* invariant:
--   the number of Apply arguments in the Agda term equals the number of arguments
--   in the resulting STyApp.
--
-- Regression prevented:
--   - silently dropping type arguments (producing STyVar instead of STyApp),
--   - or compiling only some of the Apply args.
prop_compileTypeTermWith_varApp_arity :: Property
prop_compileTypeTermWith_varApp_arity = property $ do
  k <- forAll (Gen.int (Range.linear 0 5))

  let env   = TyEnv [Just "A"]
      argT  = Var 0 []  -- a type variable as an argument
      elims = replicate k (Apply (Arg (error "ArgInfo not needed") argT))
      t     = Var 0 elims

  case compileTypeTermWith env t of
    Left _ -> failure
    Right ty -> case ty of
      STyVar _      -> k === 0
      STyApp _ args -> length args === k
      _             -> failure

-- | De Bruijn index shifting across term binders in types.
--
-- Agda uses ONE de Bruijn index space for both type and term binders.
-- Example:
--   id : {A : Type u} -> A -> A
-- The return type 'A' is often represented as Var 1, because the term binder
-- (x : A) becomes the most recent binder (Var 0) and shifts 'A' to index 1.
--
-- Our TyEnv stores term binders as Nothing specifically to preserve this alignment.
--
-- This property asserts the observable consequence:
--   in an environment [Nothing, Just "A"], Var 1 must resolve to STyVar "A".
--
-- Regression prevented: generating "t1" for polymorphic return types (e.g. def id[A](x: A): t1).
prop_compileTypeTermWith_shifted_index :: Property
prop_compileTypeTermWith_shifted_index = property $ do
  -- After one term binder, original type param "A" shifts from index 0 to index 1.
  let env = TyEnv [Nothing, Just "A"]
  compileTypeTermWith env (Var 1 []) === Right (STyVar "A")
