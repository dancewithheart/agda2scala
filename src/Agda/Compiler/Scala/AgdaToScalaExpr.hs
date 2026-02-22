module Agda.Compiler.Scala.AgdaToScalaExpr ( compileDefn ) where
  
import Agda.Compiler.Backend ( funCompiled, funClauses, Defn(..), RecordData(..))
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..), NamedName, WithOrigin(..), Ranged(..) )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), Dom'(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )
import Agda.TypeChecking.Telescope ( teleNamedArgs, teleArgs, teleArgNames )
import Agda.TypeChecking.Substitute (absBody)

import Agda.Syntax.Common (Hiding(..), getHiding)
import Agda.Syntax.Common.Pretty ( prettyShow )

import Agda.Compiler.Scala.ScalaExpr ( ScalaName
  , ScalaType(..)
  , ScalaTerm(..)
  , ScalaExpr(..)
  , SeVar(..)
  , scalaTypeScheme )

compileDefn :: Definition -> CompilerPragma -> ScalaExpr
compileDefn Defn{theDef = theDef, defName = qn, defType = dt} _pragma = case theDef of
  Datatype{dataCons = dc} -> compileDataType qn dc
  Function{funCompiled = funCompiled, funClauses = funClauses} ->
    compileFunction qn dt funCompiled funClauses
  RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
    compileRecord qn recFields recTel
  other ->
    SeUnhandled "compileDefn other" (show qn ++ "\n = \n" ++ show theDef)

compileRecord :: QName -> [Dom QName] -> Telescope -> ScalaExpr
compileRecord defName _recFields recTel = SeProd (fromQName defName) (foldr (\dt xs -> varsFromDom dt : xs) [] recTel)

varsFromDom :: Dom Type -> SeVar
varsFromDom dt = SeVar (nameFromDom dt) (STyName (fromDom dt))

compileDataType :: QName -> [QName] -> ScalaExpr
compileDataType defName fields = SeSum (fromQName defName) (map fromQName fields)

compileFunction :: QName
  -> Type
  -> Maybe CompiledClauses
  -> [Clause]
  -> ScalaExpr
compileFunction defName defTy funCompiled _funClauses =
  SeFun (fromQName defName) args (scalaTypeScheme retTy) (compileFunctionBody funCompiled)
  where
    (args, retTy) = funArgsAndReturnFromType defTy

funArgsAndReturnFromType :: Type -> ([SeVar], ScalaType)
funArgsAndReturnFromType ty0 = go 0 ty0
  where
    go :: Int -> Type -> ([SeVar], ScalaType)
    go i ty =
      case ty of
        El _ t -> goTerm i t
        _      -> ([], STyName (fromType ty))  -- fallback

    goTerm :: Int -> Term -> ([SeVar], ScalaType)
    goTerm i t = case t of
      Pi dom absTy ->
        let domTy   = STyName (fromDom dom)  -- you already have fromDom :: Dom Type -> ScalaName
            nm      = case domName dom of
                        Nothing -> "x" <> show i
                        Just a  -> namedNameToStr a
            arg     = SeVar nm domTy
            restTy  = absBody absTy
            (args, ret) = go (i + 1) restTy
        in
          -- for now: keep only explicit args
          case getHiding dom of
            Hidden -> (args, ret)               -- drop implicit for now
            _      -> (arg : args, ret)

      _ ->
        ([], STyName (fromTerm t))

nameFromDom :: Dom Type -> ScalaName
nameFromDom dt = case (domName dt) of
  Nothing -> ""
  Just a -> namedNameToStr a

-- https://hackage.haskell.org/package/Agda-2.6.4.3/docs/Agda-Syntax-Common.html#t:NamedName
namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

fromDom :: Dom Type -> ScalaName
fromDom x = fromType (unDom x)

compileFunctionResultType :: [Clause] -> ScalaType
compileFunctionResultType [Clause{clauseType = ct}] = STyName (fromMaybeType ct)
compileFunctionResultType (Clause{clauseType = ct} : xs) = STyName (fromMaybeType ct)
compileFunctionResultType other = error "Fatal error - function has not clause."

fromMaybeType :: Maybe (Arg Type) -> ScalaName
fromMaybeType (Just argType) = fromArgType argType
fromMaybeType other = error ("\nunhandled fromMaybeType \n[" ++ show other ++ "]\n")

fromArgType :: Arg Type -> ScalaName
fromArgType arg = fromType (unArg arg)

fromType :: Type -> ScalaName
fromType t = case t of
  El _ ue -> fromTerm ue
  other -> error ("unhandled fromType [" ++ show other ++ "]")

-- https://hackage.haskell.org/package/Agda-2.6.4.3/docs/Agda-Syntax-Internal.html#t:Term
fromTerm :: Term -> ScalaName
fromTerm t = case t of
  Def qName elims -> fromQName qName
  Var n elims -> "\nunhandled fromTerm Var \n[" ++ show t ++ "]\n"
  other -> error ("\nunhandled fromTerm [" ++ show other ++ "]\n")

compileFunctionBody :: Maybe CompiledClauses -> ScalaTerm
compileFunctionBody (Just funDef) = fromCompiledClauses funDef
compileFunctionBody funDef = error "Fatal error - function body is not compiled."

-- https://hackage.haskell.org/package/Agda/docs/Agda-TypeChecking-CompiledClause.html#t:CompiledClauses
fromCompiledClauses :: CompiledClauses -> ScalaTerm
fromCompiledClauses cc = case cc of
  (Case argInt _caseCompiledClauseTerm) -> STeError "WIP" --"\nCase fromCompiledClauses\n[\n" ++ (show cc) ++ "\n]\n"
  (Done (x:_xs) _term) -> fromArgName x
  other               -> STeError ("\nunhandled fromCompiledClauses \n\n[" ++ show other ++ "]\n")

fromArgName :: Arg ArgName -> ScalaTerm
fromArgName an = STeVar (unArg an)

fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
