module Agda.Compiler.Scala.PrintScala2 (
    printScala2,
    printCaseObject,
    printSealedTrait,
    printPackageAndObject,
    printCaseClass,
    printSum,
    printType,
    combineLines,
    escapeScalaString,
) where

import Data.List (dropWhileEnd, intercalate)

import Agda.Compiler.Scala.ScalaExpr (
    ScalaCtor (..),
    ScalaExpr (..),
    ScalaName,
    ScalaTerm (..),
    ScalaType (..),
    ScalaTypeScheme (..),
    SeVar (..),
 )

printScala2 :: ScalaExpr -> String
printScala2 def = case def of
    SePackage pNames defs ->
        printPackageAndObject pNames
            <> nl
            <> bracket
                (nl <> combineLines (map printScala2 defs))
            <> nl
    SeSum name tyParams ctors ->
        printSum name tyParams ctors
            <> nl
    SeFun fName args resScheme body ->
        "def"
            <> sp
            <> fName
            <> printTyParams (ssTyParams resScheme)
            <> "("
            <> intercalate ", " (map printVar args)
            <> ")"
            <> ":"
            <> sp
            <> printType (ssType resScheme)
            <> sp
            <> "="
            <> sp
            <> printTerm body
            <> nl
    SeProd name tyParams args ->
        printCaseClass name tyParams args <> nl
    SeUnhandled "" _payload ->
        "" -- filtered out
    SeUnhandled name payload ->
        "/* TODO In printScala2 got SeUnhandled " <> show name <> " " <> show payload <> " */" <> nl

-- ===== Sum types ============================================================

printSum :: ScalaName -> [ScalaName] -> [ScalaCtor] -> String
printSum name tyParams ctors =
    printSealedTrait name
        <> printTyParams tyParams
        <> nl
        <> printCompanionObject name (map (printCtor name tyParams) ctors)

printCtor :: ScalaName -> [ScalaName] -> ScalaCtor -> String
printCtor superName tyParams (ScalaCtor cName []) =
    printCaseObject (superName <> printTyParams (asBottom tyParams)) cName
printCtor superName tyParams (ScalaCtor name argTys) =
    "final case class"
        <> sp
        <> name
        <> printTyParams tyParams
        <> "("
        <> intercalate ", " (zipWith ctorParam [0 :: Int ..] argTys)
        <> ")"
        <> sp
        <> "extends"
        <> sp
        <> superName
        <> printTyParams tyParams

asBottom :: [ScalaName] -> [ScalaName]
asBottom ps = replicate (length ps) "Nothing"

ctorParam :: Int -> ScalaType -> String
ctorParam i ty = "x" <> show i <> ":" <> sp <> printType ty

printSealedTrait :: ScalaName -> String
printSealedTrait adtName = "sealed trait" <> sp <> adtName

printCaseObject :: ScalaName -> ScalaName -> String
printCaseObject superName caseName =
    "case object" <> sp <> caseName <> sp <> "extends" <> sp <> superName

-- ===== Product types ========================================================

printCaseClass :: ScalaName -> [ScalaName] -> [SeVar] -> String
printCaseClass name tyParams args =
    "final case class"
        <> sp
        <> name
        <> printTyParams tyParams
        <> "("
        <> intercalate ", " (map printVar args)
        <> ")"

-- ===== Terms ================================================================

printTerm :: ScalaTerm -> String
printTerm x = case x of
    STeVar n -> n
    STeApp f xs ->
        printTerm f <> "(" <> intercalate ", " (map printTerm xs) <> ")"
    STeLam names body ->
        "(" <> intercalate ", " names <> ")" <> sp <> "=>" <> sp <> printTerm body
    STeLitInt n -> show n
    STeLitBool b -> if b then "true" else "false" -- Scala lowercase
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
    _
        | c < ' ' -> unicodeEscape c
        | otherwise -> [c]
  where
    unicodeEscape ch =
        let n = fromEnum ch
            hex = "0123456789abcdef"
            h k = hex !! ((n `div` (16 ^ k)) `mod` 16)
         in ['\\', 'u', h 3, h 2, h 1, h 0]

-- ===== Types ================================================================

printType :: ScalaType -> String
printType (STyName n) = n
printType (STyVar v) = v
printType (STyApp n ts) =
    n <> "[" <> intercalate ", " (map printType ts) <> "]"
printType (STyFun a b) =
    printType a <> " => " <> printType b

printTyParams :: [ScalaName] -> String
printTyParams [] = ""
printTyParams ps = "[" <> intercalate ", " ps <> "]"

-- ===== Vars / packages ======================================================

printVar :: SeVar -> String
printVar (SeVar sName sType) = sName <> ":" <> sp <> printType sType

printPackageAndObject :: [ScalaName] -> String
printPackageAndObject [] = ""
printPackageAndObject [oname] = printObject oname
printPackageAndObject pName =
    printPackage (init pName)
        <> nl
        <> nl
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
    "object"
        <> sp
        <> name
        <> sp
        <> "{\n"
        <> indentBlock 2 (combineLines ctorLines)
        <> "\n"
        <> "}"

indentBlock :: Int -> String -> String
indentBlock n =
    unlines . map (replicate n ' ' <>) . lines
