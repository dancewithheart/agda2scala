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
  , unDom
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
            Just <$> lowerOneVariableEquality
              (fromQName (defName def))
              binders
              elims
      _ -> Right Nothing

lowerOneVariableEquality :: String
  -> [(Dom Type, Abs Type)]
  -> [Elim' Term]
  -> Either CompileError ScalaProperty
lowerOneVariableEquality name binders elims = do
  argument <-
    case binders of
      [(dom, _)]
        | getHiding dom == NotHidden -> do
            ty <- compileDomTypeWith emptyTyEnv dom
            case ty of
              STyName "Long" -> Right (SeVar "x0" ty)
              _ -> Left (UnsupportedProperty "Only Nat/Long generators are supported")
      _ -> Left (UnsupportedProperty "Expected exactly one explicit argument")
  (lhs, rhs) <-
    case reverse elims of
      Apply rhs : Apply lhs : rest
        | getHiding lhs == NotHidden
        , getHiding rhs == NotHidden
        , all isApply rest -> Right (unArg lhs, unArg rhs)
      _ -> Left (UnsupportedProperty "Unexpected builtin equality arguments")
  let env = envFromArgs ["x0"]
  left  <- compileBodyTerm env lhs
  right <- compileBodyTerm env rhs
  Right ScalaProperty
    { spName = name
    , spArg = argument
    , spLeft = left
    , spRight = right
    }
  where
    isApply (Apply _) = True
    isApply _ = False
