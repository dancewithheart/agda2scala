module Agda.Compiler.Scala.NameEnv
 ( NameEnv(..)
 , emptyNameEnv
 , sanitizeScalaIdent
 , allocFreshLocal
 , allocQName
 ) where

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.HashSet (HashSet)
import           Data.HashMap.Strict (HashMap)

import Agda.Syntax.Abstract.Name ( QName )

import Agda.Compiler.Scala.ScalaExpr ( ScalaName )

scalaKeywords :: HashSet String
scalaKeywords = HS.fromList
  [ "type","val","var","def","class","object","trait","enum","given","using"
  , "match","case","if","then","else","for","yield","do","while","try","catch","finally"
  , "new","this", "extends","with","import","package","private","override"
  , "true","false","null"
  ]

data NameEnv = NameEnv
  { neQNameToScala :: HashMap QName ScalaName  -- stable mapping for globals
  , neTaken        :: HashSet ScalaName        -- names already used in this module
  , neCounter      :: Int                      -- deterministic freshening
  } deriving (Eq, Show)

emptyNameEnv :: NameEnv
emptyNameEnv = NameEnv
  { neQNameToScala = HM.empty
  , neTaken        = HS.empty
  , neCounter      = 0
  }

sanitizeScalaIdent :: String -> String
sanitizeScalaIdent s0 =
  let s1 = map (\c -> if Char.isAlphaNum c then c else '_') s0
      s2 = if null s1 then "x" else s1
      s3 = if Char.isLetter (head s2) || head s2 == '_' then s2 else ('x' : s2)
      s4 = if HS.member s3 scalaKeywords then s3 <> "_" else s3
  in s4

freshen :: NameEnv -> ScalaName -> (NameEnv, ScalaName)
freshen ne base0 =
  let base = sanitizeScalaIdent base0
      candidate k =
        if k == 0 then base else base <> "_" <> show k

      go k =
        let c = candidate k
        in if HS.member c (neTaken ne) || HS.member c scalaKeywords
           then go (k + 1)
           else c

      name = go 0
      ne'  = ne { neTaken = HS.insert name (neTaken ne) }
  in (ne', name)

allocQName :: NameEnv -> QName -> String -> (NameEnv, ScalaName)
allocQName ne qn suggestedBase =
  case HM.lookup qn (neQNameToScala ne) of
    Just existing -> (ne, existing)
    Nothing ->
      let (ne1, n) = freshen ne suggestedBase
          ne2      = ne1 { neQNameToScala = HM.insert qn n (neQNameToScala ne1) }
      in (ne2, n)

allocFreshLocal :: NameEnv -> String -> (NameEnv, ScalaName)
allocFreshLocal ne suggested = freshen ne suggested
