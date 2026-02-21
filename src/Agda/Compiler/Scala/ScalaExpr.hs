module Agda.Compiler.Scala.ScalaExpr (
 ScalaName,
 ScalaType(..),
 ScalaExpr(..),
 SeVar(..),
 ScalaTerm(..),
 unHandled
 ) where

type ScalaName = String

data ScalaType
  = STyName ScalaName            -- Int, MyType
  | STyFun ScalaType ScalaType   -- Int => String
  deriving (Eq, Show)

data ScalaTerm
  = STeVar ScalaName
  | STeApp ScalaTerm [ScalaTerm]
  | STeLam [ScalaName] ScalaTerm
  | STeLitInt Int
  | STeLitBool Bool
  | STeLitString String
  | STeError String
  deriving (Eq, Show)

data SeVar = SeVar ScalaName ScalaType
  deriving (Eq, Show)

{- Represent Scala language extracted from internal Agda compiler representation -}
data ScalaExpr
  = SePackage [ScalaName] [ScalaExpr]
  | SeSum ScalaName [ScalaName]
  | SeProd ScalaName [SeVar]
  | SeFun ScalaName [SeVar] ScalaType ScalaTerm
  | SeUnhandled ScalaName String
  deriving (Eq, Show)

unHandled :: ScalaExpr -> Bool
unHandled (SeUnhandled _ _) = True
unHandled _               = False
