{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.Compile.Terms
  ( Env (..)
  , compileBodyTerm
  , compileFunctionBody
  , envFromArgs
  , envFromFunction
  , extendEnv
  , freshPatVars
  , lookupVar
  , lookupCaseArg
  , removeCaseArg
  , replaceCaseArg
) where

import Data.Maybe (catMaybes)
--import Debug.Trace (trace) -- TODO #72
import qualified Data.Map as Map
import qualified Data.Set as Set
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

newtype Env = Env { unEnv :: [Maybe ScalaName] }
  deriving (Eq, Show)

-- Args come in source order; we want env[0] = last binder (de Bruijn 0).
envFromArgs :: [ScalaName] -> Env
envFromArgs names = Env (map Just (reverse names))

envFromFunction :: [ScalaName] -> [ScalaName] -> Env
envFromFunction tyParams argNames =
  Env (map Just (reverse argNames) <> replicate (length tyParams) Nothing)

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
extendEnv names (Env xs) = Env (map Just (reverse names) <> xs)

lookupVar :: Env -> Int -> Either CompileError ScalaName
-- lookupVar env@(Env xs) i = case drop i xs of -- #72
lookupVar (Env xs) i = case drop i xs of
  Just name : _ -> Right name
  Nothing : _ -> Left (ErasedVarReferenced i)
-- -- TODO restore and hide behing a flag https://github.com/dancewithheart/agda2scala/issues/72
--    trace
--     ( "ErasedVarReferenced "
--       <> show i
--       <> " in env "
--       <> show env
--     )
--     (Left (ErasedVarReferenced i))
  [] -> Left (VarOutOfRange i (length xs))

-- Agda uses two index conventions here.
--
--   Var i  uses de Bruijn order: newest binder first.
--   Case i identifies an argument in the current source-order context.
--
-- A constructor branch replaces the scrutinized argument with constructor
-- fields at the same source-order position. A catch-all retains the argument.
lookupCaseArg :: Env -> Int -> Either CompileError ScalaName
lookupCaseArg (Env xs) i =
    case drop i (reverse xs) of
        Just name : _ -> Right name
        Nothing : _   -> Left (ErasedVarReferenced i)
        []            -> Left (VarOutOfRange i (length xs))

-- Replace the scrutinized argument, identified in source-order Case indexing,
-- with constructor fields in their source order.
replaceCaseArg
  :: Env
  -> Int
  -> [ScalaName]
  -> Either CompileError Env
replaceCaseArg (Env xs) i names =
  case splitAt i (reverse xs) of
    (before, Just _ : after) ->
      Right (Env (reverse (before <> map Just names <> after)))
    (_before, Nothing : _after) ->
      Left (ErasedVarReferenced i)
    _ ->
      Left (VarOutOfRange i (length xs))

removeCaseArg :: Env -> Int -> Either CompileError Env
removeCaseArg env i = replaceCaseArg env i []

-- ===== Function bodies =======================================================

compileFunctionBody
  :: [ScalaName]
  -> [ScalaName]
  -> Maybe CompiledClauses
  -> Either CompileError ScalaTerm
compileFunctionBody _tyParams _argNames Nothing = Left UnsupportedCompiledClauses
compileFunctionBody tyParams argNames (Just cc) = compileCompiledClauses Nothing (envFromFunction tyParams argNames) cc

-- The optional ScalaTerm is the catch-all inherited from an enclosing case.
compileCompiledClauses
  :: Maybe ScalaTerm
  -> Env
  -> CompiledClauses
  -> Either CompileError ScalaTerm
compileCompiledClauses inheritedFallback env = \case
  Done _ term -> compileBodyTerm env term
  Fail _ ->
    case inheritedFallback of
      Just fallback -> Right fallback
      Nothing       -> Left UnsupportedCompiledClauses
  Case arg branches -> do
    validateCaseShape branches
    let caseIndex = unArg arg
    scrut <- STeVar <$> lookupCaseArg env caseIndex
    caseFallback <-
      case catchallBranch branches of
        Nothing ->
          Right inheritedFallback
        Just catchallClauses ->
          Just <$> compileCompiledClauses inheritedFallback env catchallClauses
    constructorAlts <-
      traverse
        (compileConBranch caseFallback)
        (Map.toList (conBranches branches))
    let fallbackAlts =
          case caseFallback of
            Nothing       -> []
            Just fallback -> [(SPWild, fallback)]
    pure (STeMatch scrut (constructorAlts <> fallbackAlts))
    where
      compileConBranch
        :: Maybe ScalaTerm
        -> (QName, WithArity CompiledClauses)
        -> Either CompileError (ScalaPat, ScalaTerm)
      compileConBranch fallback (conQName, WithArity arityN cc) = do
        let patVars = freshPatVars env arityN
            pat = SPCtor (fromQName conQName) (map SPVar patVars)
        branchEnv <- replaceCaseArg env (unArg arg) patVars
        rhs <- compileCompiledClauses fallback branchEnv cc
        pure (pat, rhs)

freshPatVars :: Env -> Int -> [ScalaName]
freshPatVars (Env xs) arityN =
  take arityN [name | i <- [0 :: Int ..], let name = "p" <> show i, name `Set.notMember` used]
  where
    used = Set.fromList (catMaybes xs)

-- Reject unsupported case-tree shapes explicitly.
-- Silent branch dropping would generate partial Scala matches.
validateCaseShape :: Case CompiledClauses -> Either CompileError ()
validateCaseShape branches
  | projPatterns branches                 = Left (UnsupportedCaseShape HasProjectionPatterns)
  | not (Map.null (litBranches branches)) = Left (UnsupportedCaseShape HasLiteralBranches)
  | otherwise                             = Right ()

-- ===== Terms ================================================================

compileBodyTerm :: Env -> Term -> Either CompileError ScalaTerm
compileBodyTerm env = \case
    Var i elims -> do
        f <- STeVar <$> lookupVar env i
        applyElims env f elims
    Def qn elims
      | fromQName qn == "if_then_else_" -> compileIfThenElse env qn elims
      | fromQName qn == "_<ᵇ_" -> compileBinaryOp env qn "<" elims
      | fromQName qn == "_<_" -> compileBinaryOp env qn "<" elims
      | otherwise -> do
--          debugElims ("Def " <> fromQName qn) elims
          let f = STeVar (termName defaultNamePolicy (fromQName qn))
          applyElims env f elims
    Con ch _ es -> compileConApp env ch es
    Lit lit -> compileLiteral lit
    t -> Left (UnsupportedTerm t)

compileIfThenElse :: Env -> QName -> [Elim' Term] -> Either CompileError ScalaTerm
compileIfThenElse env qn elims = do
--  debugElims "if_then_else_" elims
  args <- compileVisibleApplyTerms env elims
  case args of
    [cond, thenBranch, elseBranch] ->
      pure (STeIf cond thenBranch elseBranch)
    _ -> Left (UnsupportedTerm (Def qn elims))

compileBinaryOp :: Env -> QName -> ScalaName -> [Elim' Term] -> Either CompileError ScalaTerm
compileBinaryOp env qn op elims = do
--    debugElims ("binary op " <> op) elims
    args <- compileVisibleApplyTerms env elims
    case args of
        [lhs, rhs] -> pure (STeBinOp lhs op rhs)
        _ -> Left (UnsupportedTerm (Def qn elims))

compileVisibleApplyTerms :: Env -> [Elim' Term] -> Either CompileError [ScalaTerm]
compileVisibleApplyTerms env elims = fmap catMaybes (traverse (compileVisibleElim env) elims)

compileVisibleElim :: Env -> Elim' Term -> Either CompileError (Maybe ScalaTerm)
compileVisibleElim env elim = case elim of
    Apply arg
      | getHiding arg /= NotHidden -> Right Nothing
      | isErasedTypeArgument (unArg arg) -> Right Nothing
      | otherwise -> Just <$> compileBodyTerm env (unArg arg)
    _ -> Right Nothing

compileConApp :: Env -> ConHead -> [Elim' Term] -> Either CompileError ScalaTerm
compileConApp env conHead elims = do
    args <- compileVisibleApplyTerms env elims
    let f = STeVar (ctorName defaultNamePolicy (fromQName (conName conHead)))
    pure $ case args of
        [] -> f
        _  -> STeApp f args

applyElims :: Env -> ScalaTerm -> [Elim' Term] -> Either CompileError ScalaTerm
applyElims env f elims = do
  args <- compileVisibleApplyTerms env elims
  pure $ case args of
    [] -> f
    _  -> STeApp f args

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

-- TODO restore and hide behing a flag #72
--debugElims :: String -> [Elim' Term] -> Either CompileError ()
--debugElims label elims =
--    trace
--        ( unlines
--            [ "===== AGDA2SCALA TERMS DEBUG: " <> label <> " ====="
--            , "elim count: " <> show (length elims)
--            , unlines (zipWith showElim [0 :: Int ..] elims)
--            , "===== END TERMS DEBUG ====="
--            ]
--        )
--        (Right ())
--  where
--    showElim i elim =
--        case elim of
--            Apply arg ->
--                show i
--                    <> ": Apply hiding="
--                    <> show (getHiding arg)
--                    <> " term="
--                    <> show (unArg arg)
--            _ -> show i <> ": non-Apply " <> show elim
