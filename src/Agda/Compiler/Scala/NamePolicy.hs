module Agda.Compiler.Scala.NamePolicy
  ( NamePolicy(..)
  , defaultNamePolicy
  , ctorName
  , typeName
  , termName
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import Agda.Compiler.Scala.NameEnv (sanitizeScalaIdent)
import Agda.Compiler.Scala.ScalaExpr (ScalaName)

-- | Naming policy is pure and explicit:
--   1) first apply a small mapping table for known Agda spellings
--   2) then sanitize for Scala lexical rules + keywords.
data NamePolicy = NamePolicy
  { npCtorMap :: HashMap ScalaName ScalaName
  , npTypeMap :: HashMap ScalaName ScalaName
  , npTermMap :: HashMap ScalaName ScalaName
  }

defaultNamePolicy :: NamePolicy
defaultNamePolicy = NamePolicy
  { npCtorMap = HM.fromList
      [ ("[]", "Nil")
      , ("_∷_", "Cons")
      , ("_::_", "Cons")
      , ("_∘_", "_compose_")
      , ("_,_", "Pair")
      ]
  , npTypeMap = HM.empty
  , npTermMap = HM.empty
  }

ctorName :: NamePolicy -> ScalaName -> ScalaName
ctorName pol raw = sanitizeScalaIdent (HM.lookupDefault raw raw (npCtorMap pol))

typeName :: NamePolicy -> ScalaName -> ScalaName
typeName pol raw = sanitizeScalaIdent (HM.lookupDefault raw raw (npTypeMap pol))

termName :: NamePolicy -> ScalaName -> ScalaName
termName pol raw = sanitizeScalaIdent (HM.lookupDefault raw raw (npTermMap pol))