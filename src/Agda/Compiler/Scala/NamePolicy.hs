module Agda.Compiler.Scala.NamePolicy (
    NamePolicy (..),
    emptyNamePolicy,
    stdlibNamePolicy,
    defaultNamePolicy,
    ctorName,
    typeName,
    termName,
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Agda.Compiler.Scala.NameEnv (sanitizeScalaIdent)
import Agda.Compiler.Scala.ScalaExpr (ScalaName)

{- | Naming policy has mapping tables for:
  - constructor identifiers (data constructors)
  - type identifiers (type names)
  - term identifiers (top-level defs, referenced names)
-}
data NamePolicy = NamePolicy
    { npCtorMap :: HashMap ScalaName ScalaName
    , npTypeMap :: HashMap ScalaName ScalaName
    , npTermMap :: HashMap ScalaName ScalaName
    }
    deriving (Eq, Show)

emptyNamePolicy :: NamePolicy
emptyNamePolicy =
    NamePolicy
        { npCtorMap = HM.empty
        , npTypeMap = HM.empty
        , npTermMap = HM.empty
        }

-- Right-biased union: entries in 'b' override entries in 'a'.
mergeRight :: (Eq k) => HashMap k v -> HashMap k v -> HashMap k v
mergeRight a b = HM.union b a

instance Semigroup NamePolicy where
    a <> b =
        NamePolicy
            { npCtorMap = mergeRight (npCtorMap a) (npCtorMap b)
            , npTypeMap = mergeRight (npTypeMap a) (npTypeMap b)
            , npTermMap = mergeRight (npTermMap a) (npTermMap b)
            }

instance Monoid NamePolicy where
    mempty = emptyNamePolicy

-- | Mappings for symbols from Agda standard-library
stdlibNamePolicy :: NamePolicy
stdlibNamePolicy =
    mempty
        { npCtorMap =
            HM.fromList
                [ ("[]", "Nil")
                , ("_∷_", "Cons")
                , ("_::_", "Cons") -- common rename of _∷_
                , ("_,_", "Pair")
                ]
        , npTermMap =
            HM.fromList
                [ ("_∘_", "compose")
                ]
        }

defaultNamePolicy :: NamePolicy
defaultNamePolicy =
    stdlibNamePolicy

-- override default policy when needed
-- catsPolicy
-- zioPreludePolicy
-- unicodePreservingPolicy (backticks)
-- <> userOverridesPolicy

-- ===== Applying the policy ==================================================

applyPolicy :: HashMap ScalaName ScalaName -> ScalaName -> ScalaName
applyPolicy m name = HM.lookupDefault name name m

ctorName :: NamePolicy -> ScalaName -> ScalaName
ctorName pol name = sanitizeScalaIdent (applyPolicy (npCtorMap pol) name)

typeName :: NamePolicy -> ScalaName -> ScalaName
typeName pol name = sanitizeScalaIdent (applyPolicy (npTypeMap pol) name)

termName :: NamePolicy -> ScalaName -> ScalaName
termName pol name = sanitizeScalaIdent (applyPolicy (npTermMap pol) name)
