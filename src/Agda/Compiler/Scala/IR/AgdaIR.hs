module Agda.Compiler.Scala.IR.AgdaIR
  ( AgdaDecl(..)
  , AgdaData(..)
  , AgdaRecord(..)
  , AgdaFun(..)
  ) where

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor
  , ScalaMember(..)
  , ScalaName
  , ScalaTerm
  , ScalaTypeBinder
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
  , adTyParams :: [ScalaTypeBinder]
  , adCtors    :: [ScalaCtor]
  } deriving (Eq, Show)

data AgdaRecord = AgdaRecord
  { arName     :: ScalaName
  , arTyParams :: [ScalaTypeBinder]
  , arFields   :: [ScalaMember]
  } deriving (Eq, Show)

data AgdaFun = AgdaFun
  { afName   :: ScalaName
  , afArgs   :: [SeVar]
  , afScheme :: ScalaTypeScheme
  , afBody   :: ScalaTerm
  } deriving (Eq, Show)
