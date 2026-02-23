{-# LANGUAGE LambdaCase #-}

module Agda.Compiler.Scala.AgdaToScalaExpr
  ( compileDefn
  , CompileError(..)
  , compileTypeTerm
  , compileBodyTerm
  , lookupVar
  , Env(..)
  ) where

import qualified Data.Text as T

import Agda.Compiler.Backend (CompilerPragma, Defn(..), RecordData(..), funCompiled, funClauses)
import Agda.Compiler.Backend
import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Common (Hiding(..), getHiding, Arg(..), NamedName, WithOrigin(..), Ranged(..))
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Internal
  ( Abs
  , ConHead(..)
  , Dom(..)
  , Dom'(..)
  , Elim'(..)
  , Term(..)
  , Type
  , Type''(..)
  , Tele(..)
  , Telescope
  , qnameName
  , unDom
  )
import Agda.Syntax.Literal (Literal(..))
import Agda.TypeChecking.Monad (TCM, getConstInfo)
import Agda.TypeChecking.Monad.Base (Definition(..))
import Agda.TypeChecking.CompiledClause (Case, CompiledClauses(..), CompiledClauses'(..))
import Agda.TypeChecking.Substitute (absBody)

import Agda.Compiler.Scala.NameEnv (sanitizeScalaIdent)
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr(..)
  , ScalaName
  , ScalaTerm(..)
  , ScalaType(..)
  , SeVar(..)
  , scalaTypeScheme
  )

-- ===== Errors ================================================================

data CompileError
  = UnsupportedDefinition QName
  | UnsupportedType Type
  | UnsupportedTerm Term
  | UnsupportedCompiledClauses
  | VarOutOfRange Int Int
  deriving (Eq, Show)

-- ===== Entry point ===========================================================

-- | Compile an Agda 'Definition' to Scala AST or return a structured error.
-- Agda.TypeChecking.Monad.Base.Definition:
-- https://hackage.haskell.org/package/Agda/docs/Agda-TypeChecking-Monad-Base.html#t:Definition
compileDefn :: Definition -> CompilerPragma -> TCM (Either CompileError ScalaExpr)
compileDefn def _pragma = compileDefinition def

-- Keep compileDefn small: just dispatch.
compileDefinition :: Definition -> TCM (Either CompileError ScalaExpr)
compileDefinition = \case
  Defn{theDef = Datatype{dataCons = cons}, defName = qn} ->
    compileDataType qn cons

  Defn{theDef = Function{funCompiled = cc}, defName = qn, defType = ty} ->
    pure (compileFunction qn ty cc)

  Defn{theDef = RecordDefn (RecordData{_recTel = tel}), defName = qn} ->
    pure (compileRecord qn tel)

  Defn{defName = qn} ->
    pure (Left (UnsupportedDefinition qn))

-- ===== Records ===============================================================

-- | Compile a record telescope into a Scala case class.
-- Agda.Syntax.Internal.Telescope (list-like):
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Telescope
compileRecord :: QName -> Telescope -> Either CompileError ScalaExpr
compileRecord qn tel = do
  vars <- traverse compileField (zip [0 :: Int ..] (teleToList tel))
  pure (SeProd (fromQName qn) vars)
  where
    compileField (i, dom) = do
      ty <- compileDomType dom
      pure (SeVar (binderName i dom) ty)

-- Agda.Syntax.Internal.Tele / Telescope:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Tele
-- Telescope is a linked structure, not a list.
teleToList :: Telescope -> [Dom Type]
teleToList = \case
  EmptyTel        -> []
  ExtendTel dom t -> dom : teleToList (absBody t)

-- ===== Datatypes / constructors =============================================

compileDataType :: QName -> [QName] -> TCM (Either CompileError ScalaExpr)
compileDataType typeName cons = do
  eCtors <- traverse compileCtor cons
  pure $ do
    ctors <- sequence eCtors
    pure (SeSum (fromQName typeName) ctors)

compileCtor :: QName -> TCM (Either CompileError ScalaCtor)
compileCtor conQName = do
  conDef <- getConstInfo conQName
  let conTy = defType conDef
  pure $ do
    argTys <- ctorArgTypesFromType conTy
    pure ScalaCtor { scName = fromQName conQName, scArgs = argTys }

-- | Split a type into Pi binders and a result type.
-- Stops at the first non-Pi.
unrollPi :: Type -> Either CompileError ([(Dom Type, Abs Type)], Type)
unrollPi = go []
  where
    go acc = \case
      El _ (Pi dom absTy) ->
        go ((dom, absTy) : acc) (absBody absTy)
      ty ->
        pure (reverse acc, ty)

-- | Extract explicit constructor argument types.
ctorArgTypesFromType :: Type -> Either CompileError [ScalaType]
ctorArgTypesFromType ty0 = do
  (pis, _res) <- unrollPi ty0
  traverse compileDomType
    [ dom
    | (dom, _) <- pis
    , getHiding dom /= Hidden
    ]

-- ===== Functions =============================================================

compileFunction
  :: QName
  -> Type
  -> Maybe CompiledClauses
  -> Either CompileError ScalaExpr
compileFunction qn defTy mcc = do
  (args, retTy) <- funArgsAndReturnFromType defTy
  body          <- compileFunctionBody (argNames args) mcc
  pure (SeFun (fromQName qn) args (scalaTypeScheme retTy) body)
  where
    argNames = map (\(SeVar n _) -> n)

funArgsAndReturnFromType :: Type -> Either CompileError ([SeVar], ScalaType)
funArgsAndReturnFromType ty0 = do
  (pis, resTy) <- unrollPi ty0
  args <- traverse mkArg (zip [0 :: Int ..] pis)
  ret  <- compileType resTy
  pure (concat args, ret)
  where
    mkArg (i, (dom, _absTy)) =
      case getHiding dom of
        Hidden -> pure []  -- drop implicit for now
        _      -> do
          ty <- compileDomType dom
          pure [SeVar (binderName i dom) ty]

compileFunctionBody :: [ScalaName] -> Maybe CompiledClauses -> Either CompileError ScalaTerm
compileFunctionBody _ Nothing = Left UnsupportedCompiledClauses
compileFunctionBody argNs (Just cc) =
  case cc of
    Case{}      -> Left UnsupportedCompiledClauses
    Done _ term -> compileBodyTerm (envFromArgs argNs) term
    _           -> Left UnsupportedCompiledClauses

-- ===== Terms ================================================================

newtype Env = Env { unEnv :: [ScalaName] }
  deriving (Eq, Show) -- env[0] = last binder

envFromArgs :: [ScalaName] -> Env
envFromArgs = Env . reverse

lookupVar :: Env -> Int -> Either CompileError ScalaName
lookupVar (Env xs) i =
  case drop i xs of
    v:_ -> Right v
    []  -> Left (VarOutOfRange i (length xs))

compileBodyTerm :: Env -> Term -> Either CompileError ScalaTerm
compileBodyTerm env = \case
  Var i _      -> STeVar <$> lookupVar env i
  Def qn _     -> pure (STeVar (fromQName qn))
  Con ch _ es  -> compileConApp env ch es
  Lit lit      -> compileLiteral lit
  t            -> Left (UnsupportedTerm t)

compileConApp :: Env -> ConHead -> [Elim' Term] -> Either CompileError ScalaTerm
compileConApp env conHead elims = do
  args <- traverse (compileElim env) elims
  let f = STeVar (fromQName (conName conHead))
  pure $ case args of
    [] -> f
    _  -> STeApp f args

compileElim :: Env -> Elim' Term -> Either CompileError ScalaTerm
compileElim env = \case
  Apply a -> compileBodyTerm env (unArg a)
  _       -> Left (UnsupportedTerm (Var 0 [])) -- refine later (Proj/IApply)

compileLiteral :: Literal -> Either CompileError ScalaTerm
compileLiteral = \case
  LitNat n    -> pure (STeLitInt (fromIntegral n))
  LitWord64 n -> pure (STeLitInt (fromIntegral n))
  LitString s -> pure (STeLitString (T.unpack s))
  t           -> Left (UnsupportedTerm (Lit t)) -- for now

-- ===== Types ================================================================

-- Agda.Syntax.Internal.Dom:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Dom
compileDomType :: Dom Type -> Either CompileError ScalaType
compileDomType = compileType . unDom

-- Agda.Syntax.Internal.Type:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Type
compileType :: Type -> Either CompileError ScalaType
compileType = \case
  El _ t -> compileTypeTerm t
  t      -> Left (UnsupportedType t)

-- Agda.Syntax.Internal.Term:
-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Internal.html#t:Term
compileTypeTerm :: Term -> Either CompileError ScalaType
compileTypeTerm = \case
  Def qn _  -> Right (STyName (fromQName qn))
  Var n _   -> Right (STyVar ("t" <> show n))
  Con c _ _ -> Right (STyName (fromQName (conName c)))
  Sort _    -> Right (STyName "Type")
  t         -> Left (UnsupportedTerm t)

-- ===== Naming ===============================================================

binderName :: Int -> Dom Type -> ScalaName
binderName i dom =
  case domName dom of
    Just a  ->
      let s = sanitizeScalaIdent (namedNameToStr a)
      -- binder names should never be ""
      in if null s then ("x" <> show i) else s
    Nothing -> "x" <> show i

namedNameToStr :: NamedName -> ScalaName
namedNameToStr n = rangedThing (woThing n)

-- https://hackage.haskell.org/package/Agda/docs/Agda-Syntax-Abstract-Name.html#t:QName
fromQName :: QName -> ScalaName
fromQName = prettyShow . qnameName
