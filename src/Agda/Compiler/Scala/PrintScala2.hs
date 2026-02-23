module Agda.Compiler.Scala.PrintScala2
  ( printScala2
  , printCaseObject
  , printSealedTrait
  , printPackageAndObject
  , printCaseClass
  , printType
  , combineLines
  , escapeScalaString
  ) where

import Data.List (intercalate, dropWhileEnd)

import Agda.Compiler.Scala.ScalaExpr
  ( ScalaExpr(..)
  , ScalaCtor(..)
  , ScalaName
  , ScalaTerm(..)
  , ScalaType(..)
  , ScalaTypeScheme(..)
  , SeVar(..)
  )

printScala2 :: ScalaExpr -> String
printScala2 def = case def of
  SePackage pNames defs ->
    printPackageAndObject pNames <> nl <>
    bracket (
      nl <> combineLines (map printScala2 defs)
    )
    <> nl
  SeSum adtName ctors ->
    printSealedTrait adtName <> nl <>
    printCompanionObject adtName (map (printCtor adtName) ctors) <>
    nl

  SeFun fName args resScheme body ->
    "def" <> sp <> fName <>
    "(" <> intercalate ", " (map printVar args) <> ")" <>
    ":" <> sp <> printType (ssType resScheme) <> sp <>
    "=" <> sp <> printTerm body <>
    nl
  SeProd name args ->
    printCaseClass name args <> nl
  SeUnhandled "" _payload ->
    ""  -- filtered out
  SeUnhandled name payload ->
    "/* TODO " <> show name <> " " <> show payload <> " */" <> nl
  other ->
    "/* unsupported printScala2: " <> show other <> " */" <> nl

-- ===== Sum types ============================================================

printCtor :: ScalaName -> ScalaCtor -> String
printCtor superName (ScalaCtor cName []) =
  printCaseObject superName cName

printCtor superName (ScalaCtor cName argTys) =
  "final case class" <> sp <> cName <>
  "(" <> intercalate ", " (zipWith ctorParam [0 :: Int ..] argTys) <> ")" <>
  sp <> "extends" <> sp <> superName

ctorParam :: Int -> ScalaType -> String
ctorParam i ty = "x" <> show i <> ":" <> sp <> printType ty

printSealedTrait :: ScalaName -> String
printSealedTrait adtName = "sealed trait" <> sp <> adtName

printCaseObject :: ScalaName -> ScalaName -> String
printCaseObject superName caseName =
  "case object" <> sp <> caseName <> sp <> "extends" <> sp <> superName

-- ===== Product types ========================================================

printCaseClass :: ScalaName -> [SeVar] -> String
printCaseClass name args =
  "final case class" <> sp <> name <>
  "(" <> intercalate ", " (map printVar args) <> ")"

-- ===== Terms ================================================================

printTerm :: ScalaTerm -> String
printTerm x = case x of
  STeVar n -> n
  STeApp f xs ->
    printTerm f <> "(" <> intercalate ", " (map printTerm xs) <> ")"
  STeLam names body ->
    "(" <> intercalate ", " names <> ")" <> sp <> "=>" <> sp <> printTerm body
  STeLitInt n -> show n
  STeLitBool b -> if b then "true" else "false"   -- Scala lowercase
  STeLitString s -> "\"" <> escapeScalaString s <> "\""
  STeError err -> "sys.error(" <> "\"" <> escapeScalaString err <> "\"" <> ")"

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

-- ===== Types ================================================================

printType :: ScalaType -> String
printType (STyName n) = n
printType (STyVar v)  = v
printType (STyApp n ts) =
  n <> "[" <> intercalate ", " (map printType ts) <> "]"
printType (STyFun a b) =
  printType a <> " => " <> printType b

-- ===== Vars / packages ======================================================

printVar :: SeVar -> String
printVar (SeVar sName sType) = sName <> ":" <> sp <> printType sType

printPackageAndObject :: [ScalaName] -> String
printPackageAndObject [] = ""
printPackageAndObject [oname] = printObject oname
printPackageAndObject pName =
  printPackage (init pName) <> nl <> nl
    <> printObject (last pName)

printPackage :: [ScalaName] -> String
printPackage [] = ""
printPackage pName = "package" <> sp <> intercalate "." pName

printObject :: ScalaName -> String
printObject pName = "object" <> sp <> pName

-- ===== Formatting helpers ===================================================

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

sp, nl :: String
sp = " "
nl = "\n"

combineLines :: [String] -> String
combineLines xs = strip (unlines (filter (not . null) xs))

strip :: String -> String
strip = dropWhileEnd (== '\n')

printCompanionObject :: ScalaName -> [String] -> String
printCompanionObject name ctorLines =
  "object" <> sp <> name <> sp <> "{\n"
    <> indentBlock 2 (combineLines ctorLines) <> "\n"
    <> "}"

indentBlock :: Int -> String -> String
indentBlock n =
  unlines . map (replicate n ' ' <>) . lines

