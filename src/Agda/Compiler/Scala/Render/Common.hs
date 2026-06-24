module Agda.Compiler.Scala.Render.Common
  ( escapeScalaString
  , asBottom
  , colonSeparator
  , multiLineCommentBeg
  , multiLineCommentEnd
  , nl
  , combineLines
  , printPat
  , printType
  , printTyParams
  , sp
  , strip
  ) where

import Data.List (dropWhileEnd, intercalate)

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaName
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
    unicodeEscape :: Char -> String
    unicodeEscape ch =
        let n :: Int
            n = fromEnum ch
            hex :: String
            hex = "0123456789abcdef"
            h :: Int -> Char
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

colonSeparator :: String
colonSeparator = ":"

multiLineCommentBeg, multiLineCommentEnd :: String
multiLineCommentBeg = "/*"
multiLineCommentEnd = "*/"

strip :: String -> String
strip = dropWhileEnd (== '\n')

nl :: String
nl = "\n"

sp :: String
sp = " "

combineLines :: [String] -> String
combineLines xs = strip (unlines (filter (not . null) xs))

asBottom :: [ScalaName] -> [ScalaName]
asBottom ps = replicate (length ps) "Nothing"
