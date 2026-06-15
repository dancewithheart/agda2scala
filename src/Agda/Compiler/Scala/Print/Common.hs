module Agda.Compiler.Scala.Print.Common ( escapeScalaString ) where

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
