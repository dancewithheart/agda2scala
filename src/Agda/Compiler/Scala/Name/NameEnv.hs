module Agda.Compiler.Scala.Name.NameEnv
    ( NameEnv (..)
    , allocFreshLocal
    , allocQName
    , emptyNameEnv
    , freshNumberedNamesAvoiding
    , lookupCtorOwner
    , registerCtors
    , sanitizeScalaIdent
    , scalaKeywords
) where

import qualified Data.Char as Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Agda.Syntax.Abstract.Name (QName)
import Agda.Compiler.Scala.IR.ScalaExpr(ScalaCtor (..), ScalaName)

-- Scala 3 keywords: https://scala-lang.org/files/archive/spec/3.4/01-lexical-syntax.html#regular-keywords
-- Scala 2 keywords: https://scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#identifiers
scalaKeywords :: HashSet String
scalaKeywords =
    HS.fromList
        [ "type"
        , "val"
        , "var"
        , "def"
        , "class"
        , "object"
        , "trait"
        , "enum"
        , "given"
        , "using"
        , "match"
        , "case"
        , "if"
        , "then"
        , "else"
        , "for"
        , "yield"
        , "do"
        , "while"
        , "try"
        , "catch"
        , "finally"
        , "new"
        , "this"
        , "extends"
        , "with"
        , "import"
        , "package"
        , "private"
        , "override"
        , "true"
        , "false"
        , "null"
        ]

data NameEnv = NameEnv
    { neQNameToScala :: HashMap QName ScalaName -- stable mapping for globals
    , neTaken :: HashSet ScalaName -- names already used in this module
    , neCounter :: Int -- for deterministic freshening
    , neCtorOwner :: HashMap ScalaName ScalaName -- constructor name -> parent type
    }
    deriving (Eq, Show)

emptyNameEnv :: NameEnv
emptyNameEnv =
    NameEnv
        { neQNameToScala = HM.empty
        , neTaken = HS.empty
        , neCounter = 0
        , neCtorOwner = HM.empty
        }

sanitizeScalaIdent :: String -> String
sanitizeScalaIdent = avoidScalaKeyword
  . ensureValidStart
  . defaultIfEmpty "x"
  . map sanitizeIdentChar

sanitizeIdentChar :: Char -> Char
sanitizeIdentChar c
    | Char.isAlphaNum c = c
    | otherwise = '_'

defaultIfEmpty :: String -> String -> String
defaultIfEmpty fallback name =
    case name of
        [] -> fallback
        _  -> name

ensureValidStart :: String -> String
ensureValidStart name =
    case name of
        [] -> "x"
        c : _
            | isValidScalaIdentStart c -> name
        _ -> 'x' : name

isValidScalaIdentStart :: Char -> Bool
isValidScalaIdentStart c = Char.isLetter c || c == '_'

avoidScalaKeyword :: String -> String
avoidScalaKeyword name
    | name `HS.member` scalaKeywords = name <> "_"
    | otherwise = name

freshen :: NameEnv -> ScalaName -> (NameEnv, ScalaName)
freshen ne rawBase =
    let base = sanitizeScalaIdent rawBase
        name = firstFreshName base (neTaken ne)
        ne' = ne{neTaken = HS.insert name (neTaken ne)}
     in (ne', name)

firstFreshName :: ScalaName -> HS.HashSet ScalaName -> ScalaName
firstFreshName base taken = go 0
  where
    go :: Int -> ScalaName
    go index =
        let name = freshCandidate base index
         in if name `HS.member` taken || name `HS.member` scalaKeywords
                then go (index + 1)
                else name

freshCandidate :: ScalaName -> Int -> ScalaName
freshCandidate base index
    | index == 0 = base
    | otherwise = base <> "_" <> show index

freshNumberedNamesAvoiding
  :: HashSet ScalaName
  -> ScalaName
  -> Int
  -> [ScalaName]
freshNumberedNamesAvoiding taken prefix count =
  take count
    [ candidate | i <- [0 :: Int ..]
    , let candidate = sanitizeScalaIdent (prefix <> show i)
    , candidate `notMember` taken ]

notMember a xs = not (a `HS.member` xs)

allocQName :: NameEnv -> QName -> String -> (NameEnv, ScalaName)
allocQName ne qn suggestedBase =
    case HM.lookup qn (neQNameToScala ne) of
        Just existing -> (ne, existing)
        Nothing ->
            let (ne1, n) = freshen ne suggestedBase
                ne2 = ne1{neQNameToScala = HM.insert qn n (neQNameToScala ne1)}
             in (ne2, n)

allocFreshLocal :: NameEnv -> String -> (NameEnv, ScalaName)
allocFreshLocal ne suggested = freshen ne suggested

registerCtors :: ScalaName -> [ScalaCtor] -> NameEnv -> NameEnv
registerCtors parent ctors ne =
    ne{neCtorOwner = foldl ins (neCtorOwner ne) ctors}
  where
    ins m (ScalaCtor cName _args) = HM.insert cName parent m

lookupCtorOwner :: ScalaName -> NameEnv -> Maybe ScalaName
lookupCtorOwner c ne = HM.lookup c (neCtorOwner ne)
