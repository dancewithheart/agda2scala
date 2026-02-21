module Agda.Compiler.Scala.ScalaExpr (
 ScalaName,
 ScalaType(..),
 ScalaExpr(..),
 SeVar(..),
 ScalaTerm(..),
 unHandled
 ) where

type ScalaName = String

data ScalaTerm
  = STeVar ScalaName
  | STApp ScalaTerm [ScalaTerm]
  | STLam [ScalaName] ScalaTerm
  | STLitInt Int
  | STLitBool Bool
  | STLitString String
  | STError String
  deriving (Eq, Show)

data ScalaType
  = STyName ScalaName           -- Int, MyType
  | STyFun ScalaType ScalaType  -- Int => String
  deriving (Eq, Show)

data SeVar = SeVar ScalaName ScalaType
  deriving (Eq, Show)

{- Represent Scala language extracted from internal Agda compiler representation -}
data ScalaExpr
  = SePackage [ScalaName] [ScalaExpr]
  | SeSum ScalaName [ScalaName]
  | SeFun ScalaName [SeVar] ScalaType ScalaTerm
  | SeProd ScalaName [SeVar]
  | Unhandled ScalaName String
  deriving (Eq, Show)

unHandled :: ScalaExpr -> Bool
unHandled (Unhandled _ _) = True
unHandled _               = False
