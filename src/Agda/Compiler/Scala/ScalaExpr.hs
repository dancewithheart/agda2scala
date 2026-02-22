module Agda.Compiler.Scala.ScalaExpr (
 ScalaName,
 ScalaType(..),
 ScalaExpr(..),
 SeVar(..),
 ScalaTerm(..),
 ScalaTypeScheme(..),
 ScalaCtor(..),
 scalaTypeScheme,
 unHandled
 ) where

type ScalaName = String

data ScalaType
  = STyName ScalaName            -- Int, MyType
  | STyVar ScalaName             -- A
  | STyFun ScalaType ScalaType   -- Int => String
  | STyApp ScalaName [ScalaType] -- Either[A,B], List[A]
  deriving (Eq, Show)

data ScalaTypeScheme = ScalaTypeScheme
  { ssTyParams :: [ScalaName] -- [A,B]
  ,  ssType     :: ScalaType  -- A => B
  } deriving (Eq, Show)

scalaTypeScheme :: ScalaType -> ScalaTypeScheme
scalaTypeScheme st = ScalaTypeScheme{ ssTyParams = [], ssType = st}

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

data ScalaCtor = ScalaCtor
  { scName :: ScalaName
  , scArgs :: [ScalaType]
  } deriving (Eq, Show)

data ScalaExpr
  = SePackage [ScalaName] [ScalaExpr]
  | SeSum ScalaName [ScalaCtor]
  | SeProd ScalaName [SeVar]
  | SeFun ScalaName [SeVar] ScalaTypeScheme ScalaTerm
  | SeUnhandled ScalaName String
  deriving (Eq, Show)

unHandled :: ScalaExpr -> Bool
unHandled (SeUnhandled _ _) = True
unHandled _               = False
