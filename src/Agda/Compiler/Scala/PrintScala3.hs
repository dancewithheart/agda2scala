module Agda.Compiler.Scala.PrintScala3 ( printScala3
  , printCaseObject
  , printSealedTrait
  , printPackageAndObject
  , printCaseClass
  , combineLines
  ) where

import Data.List ( intercalate )
import Agda.Compiler.Scala.ScalaExpr ( ScalaExpr(..)
  , ScalaCtor(..)
  , ScalaName
  , ScalaTerm(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , SeVar(..) )

printScala3 :: ScalaExpr -> String
printScala3 def = case def of
  (SePackage pNames defs) ->
    printPackageAndObject pNames
      <> bracket (map printScala3 defs)
      <> blankLine -- EOF
  (SeSum adtName ctors) ->
    printEnum adtName
    <> bracketWithIndent (map printEnumCtor ctors) 2
    <> defsSeparator
  (SeFun fName args resType funBody) ->
    "def" <> exprSeparator <> fName <> printTyParams (ssTyParams resType)
    <> "(" <> combineThem (map printVar args) <> ")"
    <> ":" <> exprSeparator <> printType (ssType resType) <> exprSeparator
    <> "=" <> exprSeparator <> printTerm funBody
    <> defsSeparator
  (SeProd name args) -> printCaseClass name args <> defsSeparator
  (SeUnhandled "" payload) -> ""
  (SeUnhandled name payload) -> "TODO " ++ show name ++ " " ++ show payload
  other -> "unsupported printScala3 " ++ show other

printEnumCtor :: ScalaCtor -> String
printEnumCtor (ScalaCtor cName []) =
  "case" <> exprSeparator <> cName

printEnumCtor (ScalaCtor cName argTys) =
  "case" <> exprSeparator <> cName
  <> "(" <> intercalate ", " (zipWith ctorParam [0 :: Int ..] argTys) <> ")"

ctorParam :: Int -> ScalaType -> String
ctorParam i ty = "x" <> show i <> colonSeparator <> exprSeparator <> printType ty

printCaseClass :: ScalaName -> [SeVar] -> String
printCaseClass name args = "final case class" <> exprSeparator <> name <> "(" <> printExpr args <> ")"

printType :: ScalaType -> String
printType (STyName name) = name
printType (STyVar v)     = v
printType (STyApp n ts)  = n <> "[" <> intercalate ", " (map printType ts) <> "]"
printType (STyFun a b)   = printType a <> " => " <> printType b

printTyParams :: [ScalaName] -> String
printTyParams [] = ""
printTyParams ps = "[" <> intercalate ", " ps <> "]"

printTerm :: ScalaTerm -> String
printTerm (STeVar scalaName) = scalaName
printTerm (STeApp st sts) =
  printTerm st <> "(" <> intercalate ", " (map printTerm sts) <> ")"
printTerm (STeLam sns st) = "(" <> intercalate ", " sns <> ")" <> exprSeparator <> "=>" <> exprSeparator <> printTerm st
printTerm (STeLitInt n) = show n
printTerm (STeLitBool b) = if b then "true" else "false"
printTerm (STeLitString s) = "\"" <> escapeScalaString s <> "\""
printTerm (STeError err) = "sys.error(" <> "\"" <> escapeScalaString err <> "\"" <> ")"

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
  _ | c < ' '  -> unicodeEscape c
    | otherwise -> [c]
  where
    unicodeEscape ch =
      let n = fromEnum ch
          hex = "0123456789abcdef"
          h k = hex !! ((n `div` (16^k)) `mod` 16)
      in ['\\','u', h 3, h 2, h 1, h 0]

printVar :: SeVar -> String
printVar (SeVar sName sType) = sName <> colonSeparator <> exprSeparator <> (printType sType)

printExpr :: [SeVar] -> String
printExpr names = combineThem (map printVar names)

combineThem :: [String] -> String
combineThem = intercalate ", "

printSealedTrait :: ScalaName -> String
printSealedTrait adtName = "sealed trait" <> exprSeparator <> adtName

printEnum :: ScalaName -> String
printEnum adtName = "enum" <> exprSeparator <> adtName

printCaseObject :: ScalaName -> ScalaName -> String
printCaseObject superName caseName =
  "case object" <> exprSeparator <> caseName <> exprSeparator <> "extends" <> exprSeparator <> superName

printEnumCase :: ScalaName -> String
printEnumCase  caseName =
  "case" <> exprSeparator <> caseName

printPackageAndObject :: [ScalaName] -> String
printPackageAndObject [] = ""
printPackageAndObject [oname] = printObject oname
printPackageAndObject pName = printPackage (init pName)
  <> defsSeparator <> defsSeparator
  <> printObject (last pName)

printPackage :: [ScalaName] -> String
printPackage [] = ""
printPackage pNames = "package" <> exprSeparator <> intercalate "." pNames

printObject :: ScalaName -> String
printObject pName = "object" <> exprSeparator <> pName

bracket :: [String] -> String
bracket str = colonSeparator <> defsSeparator <> combineLinesWithIndent indent str

-- -- TODO Scala3 indents
bracketWithIndent :: [String] -> Int -> String
bracketWithIndent str i =
  colonSeparator <> defsSeparator <> combineLinesWithIndent (times i indent) str

defsSeparator :: String
defsSeparator = "\n"

blankLine :: String
blankLine = "\n"

exprSeparator :: String
exprSeparator = " "

colonSeparator :: String
colonSeparator = ":"

indent :: String
indent = "  "

times :: Int -> [a] -> [a]
times i s = concat $ replicate i s

strip :: String -> String
strip xs = reverse $ dropWhile (== '\n') (reverse xs)

combineLines :: [String] -> String
combineLines xs = strip $ unlines (filter (not . null) xs)

combineLinesWithIndent :: String -> [String] -> String
combineLinesWithIndent indent xs = strip $ unlines (fmap (indent ++) (filter (not . null) xs))
