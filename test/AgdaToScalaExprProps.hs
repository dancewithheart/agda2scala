{-# LANGUAGE OverloadedStrings #-}

module AgdaToScalaExprProps (tests) where

import qualified Data.Text as T
import Hedgehog
  ( Group(..)
  , Property
  , property
  , forAll
  , (===)
  , Range
  , Gen
  , success
  , failure
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
import Agda.Compiler.Scala.AgdaToScalaExpr.Types (TyEnv(..), compileTypeTermWith)

import Agda.Compiler.Scala.ScalaExpr (ScalaTerm(..), ScalaType(..))

import Agda.Syntax.Common (Arg(..))
import Agda.Syntax.Internal (Term(..), Elim'(..))
import Agda.Syntax.Literal (Literal(..))

tests :: Group
tests =
  Group "AgdaToScalaExprProps"
    [ ("prop_lookupVar_inRange", prop_lookupVar_inRange)
    , ("prop_lookupVar_outOfRange", prop_lookupVar_outOfRange)
    , ("prop_compileTypeTerm_var_isTyVar", prop_compileTypeTerm_var_isTyVar)
    , ("prop_compileBodyTerm_lits", prop_compileBodyTerm_lits)
    , ("prop_compileBodyTerm_varResolves", prop_compileBodyTerm_varResolves)
    , ("prop_compileTypeTerm_total_onSubset", prop_compileTypeTerm_total_onSubset)
    , ("prop_typeVar_roundtrip", prop_typeVar_roundtrip)
    , ("prop_compileBodyTerm_varLaw", prop_compileBodyTerm_varLaw)
    , ("prop_compileBodyTerm_literals", prop_compileBodyTerm_literals)
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

genTypeTerm :: Gen Term
genTypeTerm =
    Gen.choice
      [ Var <$> Gen.int (Range.linear 0 20) <*> pure []
      , pure (Sort (error "Sort payload not inspected in compileTypeTerm"))  -- if compileTypeTerm ignores payload
      -- You can add Def/Con later once you have QName/ConHead generators
      ]

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

-- smoke property for compileTypeTerm on a generated subset
prop_compileTypeTerm_total_onSubset :: Property
prop_compileTypeTerm_total_onSubset = property $ do
  t <- forAll genTypeTerm
  -- should never crash; always returns Either
  case compileTypeTerm t of
    Left _  -> success
    Right _ -> success

-- Var n in types maps to STyVar
prop_typeVar_roundtrip :: Property
prop_typeVar_roundtrip = property $ do
  n <- forAll (Gen.int (Range.linear 0 100))
  compileTypeTerm (Var n []) === Right (STyVar ("t" <> show n))

prop_compileBodyTerm_varLaw :: Property
prop_compileBodyTerm_varLaw = property $ do
  xs <- forAll (Gen.list (Range.linear 1 50) genName)
  i  <- forAll (Gen.int (Range.linear 0 (length xs - 1)))
  compileBodyTerm (Env xs) (Var i []) === Right (STeVar (xs !! i))

prop_compileBodyTerm_literals :: Property
prop_compileBodyTerm_literals = property $ do
  env <- forAll genEnv
  n <- forAll (Gen.integral (Range.linear 0 100000 :: Range Integer))
  compileBodyTerm env (Lit (LitNat n)) === Right (STeLitInt (fromIntegral n))

-- type application preserves arity for Var - don’t “lose” type arguments
prop_compileTypeTermWith_var_app_arity :: Property
prop_compileTypeTermWith_var_app_arity = property $ do
  -- TyEnv with one named type var at index 0
  let tyEnv = TyEnv [Just "A"]  -- adjust constructor/import for your Types module

  k <- forAll (Gen.int (Range.linear 0 5))
  let elims = replicate k (Apply (Arg (error "arginfo") (Var 0 [])))
      t     = Var 0 elims

  case compileTypeTermWith tyEnv t of
    Left _ -> failure
    Right ty ->
      case ty of
        STyVar _ -> k === 0
        STyApp _ args -> length args === k
        _ -> failure

