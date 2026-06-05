module Agda.Compiler.Scala.IR.Agda
  ( AgdaDecl(..)
  , AgdaData(..)
  , AgdaRecord(..)
  , AgdaFun(..)
  ) where

import Agda.Compiler.Scala.ScalaExpr
  ( ScalaCtor
  , ScalaName
  , ScalaTerm
  , ScalaTypeScheme
  , SeVar
  )

data AgdaDecl
  = DData   AgdaData
  | DRecord AgdaRecord
  | DFun    AgdaFun
  deriving (Eq, Show)

data AgdaData = AgdaData
  { adName     :: ScalaName
  , adTyParams :: [ScalaName]
  , adCtors    :: [ScalaCtor]
  } deriving (Eq, Show)

data AgdaRecord = AgdaRecord
  { arName     :: ScalaName
  , arTyParams :: [ScalaName]
  , arFields   :: [SeVar]
  } deriving (Eq, Show)

data AgdaFun = AgdaFun
  { afName   :: ScalaName
  , afArgs   :: [SeVar]
  , afScheme :: ScalaTypeScheme
  , afBody   :: ScalaTerm
  } deriving (Eq, Show)
