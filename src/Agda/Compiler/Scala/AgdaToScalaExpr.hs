module Agda.Compiler.Scala.AgdaToScalaExpr
  ( compileDefn
  , CompileError(..)
  , compileTypeTerm
  , compileBodyTerm
  , lookupVar
  , Env(..)
  ) where

import Agda.Compiler.Backend (CompilerPragma)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (Definition)

import Agda.Compiler.Scala.AgdaToScalaExpr.Terms
  ( Env(..)
  , compileBodyTerm
  , lookupVar
  )
import Agda.Compiler.Scala.AgdaToScalaExpr.Types
  ( CompileError(..)
  , compileTypeTerm
  )
import Agda.Compiler.Scala.Lower.AgdaToIR
  ( lowerDefinition
  )
import Agda.Compiler.Scala.Lower.IRToScala
  ( toScalaExpr
  )
import Agda.Compiler.Scala.NamePolicy
  ( defaultNamePolicy
  )
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaExpr
  )

compileDefn :: Definition -> CompilerPragma -> TCM (Either CompileError ScalaExpr)
compileDefn def pragma = do
  eDecl <- lowerDefinition def pragma
  pure (toScalaExpr defaultNamePolicy <$> eDecl)
