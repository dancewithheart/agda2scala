module Agda.Compiler.Scala.Print.Common
  ( escapeScalaString
  , printPat
  , printType
  , printTyParams
  ) where

import Data.List (intercalate)

import Agda.Compiler.Scala.ScalaExpr
  ( ScalaName
  , ScalaExpr(..)
  , ScalaPat(..)
  , ScalaType (..)
  )

-- Escaping for Scala string literal content (no surrounding quotes).
escapeScalaString :: String -> String
escapeScalaString = concatMap $ \c -> case c of
    '\\' -> "\\\\"
    '\"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\b' -> "\\b"
    '\f' -> "\\f"
    _
        | c < ' ' -> unicodeEscape c
        | otherwise -> [c]
  where
    unicodeEscape ch =
        let n = fromEnum ch
            hex = "0123456789abcdef"
            h k = hex !! ((n `div` (16 ^ k)) `mod` 16)
         in ['\\', 'u', h 3, h 2, h 1, h 0]

printType :: ScalaType -> String
printType (STyName name) = name
printType (STyVar v) = v
printType (STyApp n ts) = n <> "[" <> intercalate ", " (map printType ts) <> "]"
printType (STyFun a b) = printType a <> " => " <> printType b

printTyParams :: [ScalaName] -> String
printTyParams [] = ""
printTyParams ps = "[" <> intercalate ", " ps <> "]"

printPat :: ScalaPat -> String
printPat pat =
  case pat of
    SPWild           -> "_"
    SPVar name       -> name
    SPCtor name []   -> name
    SPCtor name args ->
      name <> "(" <> intercalate ", " (map printPat args) <> ")"
    SPLitInt n       -> show n
    SPLitBool b      -> if b then "true" else "false"
    SPLitString s    -> "\"" <> escapeScalaString s <> "\""
