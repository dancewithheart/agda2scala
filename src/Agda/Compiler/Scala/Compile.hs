module Agda.Compiler.Scala.Compile
  ( compileDefinition
  , CompileError(..)
  , compileTypeTerm
  , compileBodyTerm
  , lookupVar
  , Env(..)
  ) where

import Agda.Compiler.Backend (CompilerPragma)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (Definition)
import Agda.Compiler.Scala.Compile.Terms (Env(..), compileBodyTerm, lookupVar)
import Agda.Compiler.Scala.Compile.Types (CompileError(..), compileTypeTerm)
import Agda.Compiler.Scala.Lower.AgdaToIR (toAgdaExpr)
import Agda.Compiler.Scala.Lower.IRToScala (toScalaExpr)
import Agda.Compiler.Scala.Name.NamePolicy (defaultNamePolicy)
import Agda.Compiler.Scala.IR.ScalaExpr (ScalaExpr)

-- compile definition produced by agda compiler
-- into AgdaIR (representation close to Agda compiler)
-- and then into ScalaIR (representation close to Scala)
--            toAgdaExpr          toScalaExpr
-- Definition ----------> AgdaIR ------------> ScalaIR
compileDefinition :: Definition -> CompilerPragma -> TCM (Either CompileError ScalaExpr)
compileDefinition definition pragma = do
  agdaRep <- toAgdaExpr definition pragma
  pure (toScalaExpr defaultNamePolicy <$> agdaRep)
