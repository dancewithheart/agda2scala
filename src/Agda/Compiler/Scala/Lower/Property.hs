module Agda.Compiler.Scala.Lower.Property
  ( lowerEqualityProperty
  ) where

import Agda.Syntax.Builtin (builtinEquality)
import Agda.Syntax.Common (Arg(..), Hiding(..), getHiding)
import Agda.Syntax.Internal
  ( Abs
  , Dom
  , Elim'(..)
  , Term(..)
  , Type
  , Type''(..)
  )
import Agda.TypeChecking.Monad.Base (Definition(..))
import Agda.TypeChecking.Monad.Builtin (getBuiltinName')
import Agda.Compiler.Backend (TCM)

import Agda.Compiler.Scala.Compile.Terms
  ( compileBodyTerm
  , envFromArgs
  )
import Agda.Compiler.Scala.Compile.Types
  ( CompileError(..)
  , compileDomTypeWith
  , emptyTyEnv
  , fromQName
  , unrollPi
  )
import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaProperty(..)
  , ScalaType(..)
  , SeVar(..)
  )

lowerEqualityProperty :: Definition
  -> TCM (Either CompileError (Maybe ScalaProperty))
lowerEqualityProperty def = do
  equalityName <- getBuiltinName' builtinEquality
  pure $ do
    (binders, resultType) <- unrollPi (defType def)
    case (equalityName, resultType) of
      (Just equality, El _ (Def name elims))
        | name == equality ->
             Just <$> lowerNatEquality
               (fromQName (defName def))
               binders
               elims
      _ -> Right Nothing

lowerNatEquality :: String
  -> [(Dom Type, Abs Type)]
  -> [Elim' Term]
  -> Either CompileError ScalaProperty
lowerNatEquality name binders elims = do
  arguments <-
    case length binders of
      n | n >= 1 && n <= 3 ->
        traverse lowerBinder (zip [0 :: Int ..] binders)
      _ ->
        Left (UnsupportedProperty "Expected 1–3 arguments")
  (lhs, rhs) <-
    case reverse elims of
      Apply rhs : Apply lhs : rest
        | getHiding lhs == NotHidden
        , getHiding rhs == NotHidden
        , all isApply rest ->
            Right (unArg lhs, unArg rhs)
      _ ->
        Left (UnsupportedProperty "Unexpected builtin equality arguments")
  let env = envFromArgs [n | SeVar n _ <- arguments]
  left  <- compileBodyTerm env lhs
  right <- compileBodyTerm env rhs
  pure ScalaProperty
    { spName = name
    , spArgs = arguments
    , spLeft = left
    , spRight = right
    }
  where
    lowerBinder (i, (dom, _))
      | getHiding dom /= NotHidden =
          Left (UnsupportedProperty "Expected explicit arguments")
      | otherwise = do
          ty <- compileDomTypeWith emptyTyEnv dom
          case ty of
            STyName "Long" ->
              Right (SeVar ("x" <> show i) ty)
            _ ->
              Left (UnsupportedProperty "Only Nat/Long generators are supported")
    isApply (Apply _) = True
    isApply _ = False
