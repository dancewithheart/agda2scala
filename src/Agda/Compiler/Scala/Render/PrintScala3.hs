module Agda.Compiler.Scala.Render.PrintScala3 (
    printScala3,
    printCaseObject,
    printSealedTrait,
    printPackageAndObject,
    printCaseClass,
) where

import Agda.Compiler.Scala.Render.Common
  ( escapeScalaString
  , asBottom
  , colonSeparator
  , nl
  , printPat
  , printType
  , printTyParams
  , strip
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
import Data.List (intercalate)

printScala3 :: ScalaExpr -> String
printScala3 def = case def of
    (SePackage pNames defs) ->
        printPackageAndObject pNames
            <> bracket (map printScala3 defs)
            <> nl -- EOF
    (SeSum name tyParams ctors) ->
        printSum name tyParams ctors
            <> defsSeparator
    (SeFun fName args resType funBody) ->
        "def"
            <> exprSeparator
            <> fName
            <> printTyParams (ssTyParams resType)
            <> "("
            <> combineThem (map printVar args)
            <> ")"
            <> colonSeparator
            <> exprSeparator
            <> printType (ssType resType)
            <> exprSeparator
            <> "="
            <> exprSeparator
            <> printTerm funBody
            <> defsSeparator
    (SeProd name _tyParams args) -> printCaseClass name args <> defsSeparator
    (SeUnhandled "" _payload) -> ""
    (SeUnhandled name payload) -> "/* TODO In printScala3 got SeUnhandled "
      <> show name
      <> " "
      <> show payload
      <> "*/"
      <> defsSeparator

-- ===== Sum types ============================================================

printSum :: ScalaName -> [ScalaName] -> [ScalaCtor] -> String
printSum name tyParams ctors =
    printEnum name
        <> printTyParams tyParams
        <> bracketWithIndent (map (printEnumCtor name tyParams) ctors) 2

printEnumCtor :: ScalaName -> [ScalaName] -> ScalaCtor -> String
printEnumCtor name tyParams (ScalaCtor cName []) =
    "case"
        <> exprSeparator
        <> cName
        <> printExtends name (asBottom tyParams)
printEnumCtor name tyParams (ScalaCtor cName argTys) =
    "case"
        <> exprSeparator
        <> cName
        <> printTyParams tyParams
        <> "("
        <> intercalate ", " (zipWith ctorParam [0 :: Int ..] argTys)
        <> ")"
        <> printExtends name tyParams

printExtends :: ScalaName -> [ScalaName] -> String
printExtends _ [] = ""
printExtends name tyParams = " extends " <> name <> printTyParams tyParams

ctorParam :: Int -> ScalaType -> String
ctorParam i ty = "x" <> show i <> colonSeparator <> exprSeparator <> printType ty

printCaseClass :: ScalaName -> [SeVar] -> String
printCaseClass name args = "final case class" <> exprSeparator <> name <> "(" <> printExpr args <> ")"

printTerm :: ScalaTerm -> String
printTerm = printTermBlock

printTermInline :: ScalaTerm -> String
printTermInline term =
    case term of
        STeIf{} -> "(" <> printTermBlock term <> ")"
        STeMatch{} -> "(" <> printTermBlock term <> ")"
        _ -> printTermBlock term

printTermBlock :: ScalaTerm -> String
printTermBlock term =
    case term of
        STeVar scalaName -> scalaName
        STeApp st sts -> printTermInline st <> "(" <> intercalate ", " (map printTermInline sts) <> ")"
        STeLam sns st -> "(" <> intercalate ", " sns <> ")" <> exprSeparator <> "=>" <> exprSeparator <> printTermInline st
        STeLitInt n -> show n
        STeLitBool b -> if b then "true" else "false"
        STeLitString s -> "\"" <> escapeScalaString s <> "\""
        STeIf cond thenBranch elseBranch -> printIf cond thenBranch elseBranch
        STeBinOp lhs op rhs -> printTermInline lhs <> exprSeparator <> op <> exprSeparator <> printTermInline rhs
        STeError err -> "sys.error(" <> "\"" <> escapeScalaString err <> "\"" <> ")"
        STeMatch scrut alts ->
            printTermInline scrut <> sp <> "match" <> nl
                <> indentBlock 2 (intercalate "\n" (map printCase alts))

printIf :: ScalaTerm -> ScalaTerm -> ScalaTerm -> String
printIf cond thenBranch elseBranch =
    "if" <> sp <> printTermInline cond <> sp <> "then" <> nl
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
    "case" <> sp <> printPat pat <> sp <> "=>" <> nl
        <> indentBlock 2 (printTermBlock rhs)

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

printPackageAndObject :: [ScalaName] -> String
printPackageAndObject [] = ""
printPackageAndObject [oname] = printObject oname
printPackageAndObject pName =
    printPackage (init pName)
        <> defsSeparator
        <> defsSeparator
        <> printObject (last pName)

printPackage :: [ScalaName] -> String
printPackage [] = ""
printPackage pNames = "package" <> exprSeparator <> intercalate "." pNames

printObject :: ScalaName -> String
printObject pName = "object" <> exprSeparator <> pName

defsSeparator :: String
defsSeparator = "\n"

exprSeparator :: String
exprSeparator = " "

-- -- TODO Scala3 indents
bracket :: [String] -> String
bracket blocks =
    colonSeparator <> defsSeparator <> combineMemberBlocksWithIndent 2 blocks

bracketWithIndent :: [String] -> Int -> String
bracketWithIndent blocks i =
    colonSeparator <> defsSeparator <> combineBlocksWithIndent i blocks

combineMemberBlocksWithIndent :: Int -> [String] -> String
combineMemberBlocksWithIndent n blocks =
    intercalate (defsSeparator <> defsSeparator) $
        map (indentBlock n) $
            nonEmptyBlocks blocks

combineBlocksWithIndent :: Int -> [String] -> String
combineBlocksWithIndent n blocks =
    intercalate defsSeparator $
        map (indentBlock n) $
            nonEmptyBlocks blocks

nonEmptyBlocks :: [String] -> [String]
nonEmptyBlocks =
    filter (not . null) . map strip

indentBlock :: Int -> String -> String
indentBlock n =
    intercalate "\n" . map (replicate n ' ' <>) . lines
