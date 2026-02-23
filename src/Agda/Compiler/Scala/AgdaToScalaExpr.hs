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
import Agda.Syntax.Internal (Elim'(..))        -- Apply, Proj, ...
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
  , scalaTypeScheme
  , ScalaCtor(..) )

import Agda.Compiler.Scala.NameEnv ( sanitizeScalaIdent )

compileDefn :: Definition -> CompilerPragma -> TCM ScalaExpr
compileDefn Defn{theDef = theDef, defName = qn, defType = dt} _pragma = case theDef of
  Datatype{dataCons = cons}                              -> compileDataType qn cons
  Function{funCompiled = cc, funClauses = cls}           -> pure $ compileFunction qn dt cc cls
  RecordDefn(RecordData{_recFields = fs, _recTel = tel}) -> pure $ compileRecord qn fs tel
  other -> pure $ SeUnhandled "compileDefn other" (show qn ++ "\n = \n" ++ show theDef)

compileRecord :: QName -> [Dom QName] -> Telescope -> ScalaExpr
compileRecord defName _recFields recTel = SeProd (fromQName defName) (foldr (\dt xs -> varsFromDom dt : xs) [] recTel)

varsFromDom :: Dom Type -> SeVar
varsFromDom dt = SeVar (nameFromDom dt) (fromDom dt)

compileDataType :: QName -> [QName] -> TCM ScalaExpr
compileDataType typeName cons = do
  ctors <- traverse compileCtor cons
  pure $ SeSum (fromQName typeName) ctors

compileCtor :: QName -> TCM ScalaCtor
compileCtor conQName = do
  conDef <- getConstInfo conQName
  let conTy = defType conDef
      argTys = ctorArgTypesFromType conTy
  pure $ ScalaCtor
    { scName = fromQName conQName
    , scArgs = argTys
    }

-- Constructor type looks like: (a : A) -> (b : B) -> T params...
-- We want argument types only (explicit for now), stop at the return.
ctorArgTypesFromType :: Type -> [ScalaType]
ctorArgTypesFromType ty0 = go ty0
  where
    go ty = case ty of
      El _ t -> goTerm t
      _      -> []

    goTerm t = case t of
      Pi dom absTy ->
        let domTy = fromDom dom
            rest  = absBody absTy
        in case getHiding dom of
             Hidden -> go rest        -- ignore implicit args for now
             _      -> domTy : go rest
      _ ->
        []

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
        _      -> ([], fromType ty)  -- fallback

    goTerm :: Int -> Term -> ([SeVar], ScalaType)
    goTerm i t = case t of
      Pi dom absTy ->
        let domTy   = fromDom dom
            nm      = chooseBinderName i dom
            arg     = SeVar nm domTy
            restTy  = absBody absTy
            (args, ret) = go (i + 1) restTy
        in
          -- for now: keep only explicit args
          case getHiding dom of
            Hidden -> (args, ret)               -- drop implicit for now
            _      -> (arg : args, ret)

      _ ->
        ([], fromTerm t)

chooseBinderName :: Int -> Dom Type -> ScalaName
chooseBinderName i dom =
  case domName dom of
    Just a  -> sanitizeScalaIdent (namedNameToStr a)
    Nothing -> "x" <> show i

nameFromDom :: Dom Type -> ScalaName
nameFromDom dt = case (domName dt) of
  Nothing -> ""
  Just a  -> namedNameToStr a

-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Common.html#t:NamedName
namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

fromDom :: Dom Type -> ScalaType
fromDom x = fromType (unDom x)

fromArgType :: Arg Type -> ScalaType
fromArgType arg = fromType (unArg arg)

fromType :: Type -> ScalaType
fromType t = case t of
  El _ ue -> fromTerm ue
  other   -> error ("unhandled fromType [" ++ show other ++ "]")

-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Term
fromTerm :: Term -> ScalaType
fromTerm t =
  let ty = case t of
              Def qName _elims -> fromQName qName
              Var n _elims     -> "t" <> show n        -- temporary type variable name
              Con c _elims _   -> fromQName (conName c) -- constructor
              Sort _           -> "Type"
              _                -> "unhandled term: " <> take 60 (show t)
  in STyName ty

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
    Def qn _elims -> STeVar (fromQName qn)
    Con conHead _conInfo elims ->
      case args of
         [] -> f
         _  -> STeApp f args
      where
        f    = STeVar (fromQName (conName conHead))
        args = [ compileBodyTerm env (unArg a) | Apply a <- elims ]
    Lit (LitNat n)     -> STeLitInt (fromIntegral n)
    Lit (LitWord64 n)  -> STeLitInt (fromIntegral n)
    Lit (LitString s)  -> STeLitString (T.unpack s)
    _ -> STeError ("compileBodyTerm: unhandled term: " <> take 120 (show t))

fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
