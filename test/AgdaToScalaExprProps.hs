{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AgdaToScalaExprProps (tests) where

import qualified Data.Text as T
import Hedgehog
  ( Group(..)
  , Property
  , property
  , forAll
  , (===)
  , Range(..)
  , Gen(..)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.AgdaToScalaExpr
  ( CompileError(..)
  , Env(..)
  , lookupVar
  , compileTypeTerm
  , compileBodyTerm
  )

import Agda.Compiler.Scala.ScalaExpr (ScalaTerm(..), ScalaType(..))

import Agda.Syntax.Internal (Term(..))
import Agda.Syntax.Literal (Literal(..))

tests :: Group
tests =
  Group "AgdaToScalaExprProps"
    [ ("prop_lookupVar_inRange", prop_lookupVar_inRange)
    , ("prop_lookupVar_outOfRange", prop_lookupVar_outOfRange)
    , ("prop_compileTypeTerm_var_isTyVar", prop_compileTypeTerm_var_isTyVar)
    , ("prop_compileBodyTerm_lits", prop_compileBodyTerm_lits)
    , ("prop_compileBodyTerm_varResolves", prop_compileBodyTerm_varResolves)
    ]

-- ===== generators ============================================================

genEnv :: Gen Env
genEnv = do
  -- env[0] = last binder, so store in the actual runtime order already
  n  <- Gen.int (Range.linear 0 30)
  xs <- Gen.list (Range.singleton n) genName
  pure (Env xs)

genName :: Gen String
genName = Gen.string (Range.linear 1 12) Gen.alphaNum

genIndexInRange :: Env -> Gen Int
genIndexInRange (Env xs) =
  case length xs of
    0 -> Gen.discard
    k -> Gen.int (Range.linear 0 (k - 1))

genIndexOutOfRange :: Env -> Gen Int
genIndexOutOfRange (Env xs) = do
  -- produce an index >= length
  let k = length xs
  Gen.int (Range.linear k (k + 20))

-- ===== properties ============================================================

prop_lookupVar_inRange :: Property
prop_lookupVar_inRange = property $ do
  env@(Env xs) <- forAll genEnv
  i <- forAll (genIndexInRange env)
  lookupVar env i === Right (xs !! i)

prop_lookupVar_outOfRange :: Property
prop_lookupVar_outOfRange = property $ do
  env@(Env xs) <- forAll genEnv
  i <- forAll (genIndexOutOfRange env)
  lookupVar env i === Left (VarOutOfRange i (length xs))

prop_compileTypeTerm_var_isTyVar :: Property
prop_compileTypeTerm_var_isTyVar = property $ do
  n <- forAll (Gen.int (Range.linear 0 100))
  let t = Var n []     -- type-level var is represented as Term Var in your compiler
  compileTypeTerm t === Right (STyVar ("t" <> show n))

prop_compileBodyTerm_lits :: Property
prop_compileBodyTerm_lits = property $ do
  env <- forAll genEnv

  n <- forAll (Gen.integral (Range.linear 0 100000 :: Range Integer))
  compileBodyTerm env (Lit (LitNat n)) === Right (STeLitInt (fromIntegral n))

  w <- forAll (Gen.integral (Range.linear 0 100000 :: Range Word))
  compileBodyTerm env (Lit (LitWord64 (fromIntegral w))) === Right (STeLitInt (fromIntegral w))

  s <- forAll (Gen.text (Range.linear 0 50) Gen.unicode)
  compileBodyTerm env (Lit (LitString s)) === Right (STeLitString (T.unpack s))

prop_compileBodyTerm_varResolves :: Property
prop_compileBodyTerm_varResolves = property $ do
  env@(Env xs) <- forAll genEnv
  i <- forAll (genIndexInRange env)

  -- Term-level Var i should resolve to env[i]
  compileBodyTerm env (Var i []) === Right (STeVar (xs !! i))
