module Agda.Compiler.Scala.IR.ScalaIR
  ( ScalaModule(..)
  , ScalaExpr(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , ScalaTerm(..)
  , ScalaCtor(..)
  , SeVar(..)
  ) where

import Agda.Compiler.Scala.IR.ScalaExpr

data ScalaModule = ScalaModule
  { smPackage :: [ScalaName]
  , smDecls   :: [ScalaExpr]
  } deriving (Eq, Show)
