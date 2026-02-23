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
    "def" <> exprSeparator <> fName
    <> "(" <> combineThem (map printVar args) <> ")"
    <> ":" <> exprSeparator <> (printType (ssType resType)) <> exprSeparator
    <> "=" <> exprSeparator <> (printTerm funBody)
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

printTerm :: ScalaTerm -> String
printTerm (STeVar scalaName) = scalaName
printTerm (STeApp st sts) = (printTerm st) <> "(" <> (show sts)  <> ")"
printTerm (STeLam sns st) = (combineLines sns) <> " => " <> (printTerm st)
printTerm (STeLitInt n) = show n
printTerm (STeLitBool b) = show b
printTerm (STeLitString s) = show s
printTerm (STeError err) = "error " <> err

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
