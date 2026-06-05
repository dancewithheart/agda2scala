module Agda.Compiler.Scala.IR.Scala
  ( ScalaModule(..)
  , ScalaExpr(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , ScalaTerm(..)
  , ScalaCtor(..)
  , SeVar(..)
  ) where

import Agda.Compiler.Scala.ScalaExpr

data ScalaModule = ScalaModule
  { smPackage :: [ScalaName]
  , smDecls   :: [ScalaExpr]
  } deriving (Eq, Show)
