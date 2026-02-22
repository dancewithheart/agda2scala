module Agda.Compiler.Scala.AgdaToScalaExpr ( compileDefn ) where

import qualified Data.Text as T

import Agda.Compiler.Backend ( funCompiled, funClauses, Defn(..), RecordData(..))
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common (Hiding(..), getHiding)
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..), NamedName, WithOrigin(..), Ranged(..) )
import Agda.Syntax.Literal ( Literal(..) )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), Dom'(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.Syntax.Internal (Elim'(..), Elim(..))        -- Apply, Proj, ...
import Agda.Syntax.Internal (ConHead(..))     -- conName
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )
import Agda.TypeChecking.Telescope ( teleNamedArgs, teleArgs, teleArgNames )
import Agda.TypeChecking.Substitute (absBody)

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
  SeFun (fromQName defName) args (scalaTypeScheme retTy) (compileFunctionBody argNames funCompiled)
  where
    (args, retTy) = funArgsAndReturnFromType defTy
    argNames      = [ n | SeVar n _ <- args ]

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

-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Term
fromTerm :: Term -> ScalaName
fromTerm t = case t of
  Def qName _elims -> fromQName qName
  Var n _elims     -> "t" <> show n  -- temporary type variable name
  Con c _elims _   -> prettyShow c  -- constructor
  Sort _           -> "Type"
  _                -> "unhandled term: " <> take 60 (show t)

compileFunctionBody :: [ScalaName] -> Maybe CompiledClauses -> ScalaTerm
compileFunctionBody argNames (Just funDef) = fromCompiledClauses argNames funDef
compileFunctionBody _        Nothing = STeError "Function body is not compiled."

-- https://hackage.haskell.org/package/Agda/docs/Agda-TypeChecking-CompiledClause.html#t:CompiledClauses
fromCompiledClauses :: [ScalaName] -> CompiledClauses -> ScalaTerm
fromCompiledClauses argNames cc = case cc of
  (Case argInt _caseCompiledClauseTerm) -> STeError "Case (pattern matching) not implemented yet"
  (Done _ term) -> compileBodyTerm (reverse argNames) term -- reverse to get most recent binder
  other               -> STeError ("unhandled fromCompiledClauses: " <> take 60 (show other))

-- env[0] = last argument, env[1] = second last, etc.
compileBodyTerm :: [ScalaName] -> Term -> ScalaTerm
compileBodyTerm env t =
  case t of
    Var i _elims ->
      case drop i env of
        (v:_) -> STeVar v
        []    -> STeError ("Var index out of range: " <> show i)

    Def qn _elims ->
      STeVar (fromQName qn)

    Con conHead _conInfo elims ->
      case args of
         [] -> f
         _  -> STeApp f args
      where
        f    = STeVar (fromQName (conName conHead))
        args = [ compileBodyTerm env (unArg a) | Apply a <- elims ]


    -- expand these gradually:
    Lit (LitNat n)     -> STeLitInt (fromIntegral n)
    Lit (LitWord64 n)  -> STeLitInt (fromIntegral n)
    Lit (LitString s)  -> STeLitString (T.unpack s)

    _ ->
      STeError ("compileBodyTerm: unhandled term: " <> take 120 (show t))



fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
