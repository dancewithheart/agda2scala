module Agda.Compiler.Scala.Render.PrintScala2 (
    printScala2,
    printCaseObject,
    printSealedTrait,
    printPackageAndObject,
    printCaseClass,
    printSum,
    printType,
    combineDecls
) where

import Data.List (intercalate)

import Agda.Compiler.Scala.Render.Common
  ( escapeScalaString
  , asBottom
  , multiLineCommentBeg
  , multiLineCommentEnd
  , nl
  , combineLines
  , printPat
  , printType
  , printTyParams
  , sp
  )
import Agda.Compiler.Scala.IR.ScalaExpr (
    ScalaCtor (..),
    ScalaExpr (..),
    ScalaName,
    ScalaPat(..),
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
                (nl <> combineDecls (map printScala2 defs))
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
        multiLineCommentBeg
        <> " TODO In printScala2 got SeUnhandled "
        <> show name
        <> " "
        <> show payload
        <> " "
        <> multiLineCommentEnd
        <> nl

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
printTerm = printTermBlock

printTermInline :: ScalaTerm -> String
printTermInline term =
    case term of
        STeIf{} -> "(" <> printTermBlock term <> ")"
        STeMatch{} -> "(" <> printTermBlock term <> ")"
        _ -> printTermBlock term

printTermBlock :: ScalaTerm -> String
printTermBlock x = case x of
    STeVar n -> n
    STeApp f xs -> printTermInline f <> "(" <> intercalate ", " (map printTermInline xs) <> ")"
    STeLam names body -> "(" <> intercalate ", " names <> ")" <> sp <> "=>" <> sp <> printTermInline body
    STeLitInt n -> show n
    STeLitBool b -> if b then "true" else "false"
    STeLitString s -> "\"" <> escapeScalaString s <> "\""
    STeIf cond thenBranch elseBranch -> printIf cond thenBranch elseBranch
    STeBinOp lhs op rhs -> printTermInline lhs <> sp <> op <> sp <> printTermInline rhs
    STeMatch scrut alts ->
      printTermInline scrut <> sp <> "match" <> sp <> "{\n"
        <> intercalate "\n" (map (indentBlock 2 . printCase) alts)
        <> "\n}"
    STeError err -> "sys.error(" <> "\"" <> escapeScalaString err <> "\"" <> ")"

printIf :: ScalaTerm -> ScalaTerm -> ScalaTerm -> String
printIf cond thenBranch elseBranch =
    "if" <> sp <> "(" <> printTermInline cond <> ")" <> nl
        <> indentBlock 2 (printTermBlock thenBranch)
        <> nl
        <> "else"
        <> printElseBranch elseBranch

printElseBranch :: ScalaTerm -> String
printElseBranch elseBranch =
    case elseBranch of
        STeIf{} -> sp <> printTermBlock elseBranch
        _ -> nl <> indentBlock 2 (printTermBlock elseBranch)

printCase :: (ScalaPat, ScalaTerm) -> String
printCase (pat, rhs) =
  "case" <> sp <> printPat pat <> sp <> "=>"
      <> nl
      <> indentBlock 2 (printTermBlock rhs)

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

printCompanionObject :: ScalaName -> [String] -> String
printCompanionObject name ctorLines =
    "object"
        <> sp
        <> name
        <> sp
        <> bracket (indentBlock 2 (combineLines ctorLines))


indentBlock :: Int -> String -> String
indentBlock n = intercalate "\n" . map (replicate n ' ' <>) . lines

combineDecls :: [String] -> String
combineDecls =
    intercalate (nl <> nl) . filter (not . null) . map stripTrailingNewlines

stripTrailingNewlines :: String -> String
stripTrailingNewlines =
    reverse . dropWhile (== '\n') . reverse
