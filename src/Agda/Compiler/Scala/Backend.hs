{-# LANGUAGE PatternSynonyms #-}

module Agda.Compiler.Scala.Backend
  ( Options (..)
  , defaultOptions
  , initModuleEnv
  , lowerRecordConstruction
  , substituteType
  , outDirOpt
  , qualifyTermWithEnv
  , runScalaBackend
  , scalaBackend
  , scalaBackend'
  , scalaDialectOpt
  , selectPrinter
  , shouldWriteModule
  , checkCompile
  , unhandledErrors
) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List.NonEmpty as Nel
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_agda2scala (version)
import Agda.Compiler.Backend (
    Backend,
    Backend',
    Backend'_boot(..),
    CompilerPragma,
    Definition (..),
    Flag,
    IsMain,
    Recompile (..),
    TCM,
    RecordData (..),
    Defn(..),
    funWith,
    pattern Function
 )
import qualified Agda.Compiler.Backend.Base as BackendBase
import Agda.Compiler.Common (compileDir)
import Agda.Main (runAgda)
import Agda.Syntax.Abstract.Name( QName(..) )
import Agda.Syntax.Common (moduleNameParts)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Internal (ConHead (..))
import Agda.Syntax.TopLevelModuleName (TopLevelModuleName, moduleNameToFileName)
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.TypeChecking.Monad.Signature (getUniqueCompilerPragma)
import Agda.Utils.GetOpt (ArgDescr (ReqArg), OptDescr (Option))

import Agda.Compiler.Scala.Compile (CompileError (..), compileDefinition)
import Agda.Compiler.Scala.Name.NameEnv
  ( NameEnv
  , emptyNameEnv
  , isNullaryTerm
  , lookupCtorOwner
  , lookupRecordCtor
  , registerCtors
  , registerRecordCtor
  , registerNullaryTerm
  , TraitCtor (..)
  , lookupTraitCtor
  , registerTraitCtor
  )
import Agda.Compiler.Scala.Name.NamePolicy (defaultNamePolicy, ctorName)
import Agda.Compiler.Scala.Render.PropertyTest (printPropertySuite)
import Agda.Compiler.Scala.Render.PrintScala2 (printScala2, printScala2Term)
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3, printScala3Term)

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaExpr (..)
  , ScalaInstanceMember (..)
  , ScalaMember (..)
  , ScalaPat (..)
  , ScalaTerm (..)
  , ScalaType(..)
  , ScalaTypeBinder (..)
  , ScalaTypeScheme(..)
  , SeVar(..)
  , unHandled
  )
import Agda.Compiler.Scala.Compile.Types (fromQName, substituteType)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), dropExtension, takeDirectory)

runScalaBackend :: IO ()
runScalaBackend = runAgda [scalaBackend]

scalaBackend :: Backend
scalaBackend = BackendBase.Backend scalaBackend'

data Options = Options
  { optOutDir     :: Maybe FilePath
  , optTestOutDir :: Maybe FilePath
  , scalaDialect  :: Maybe String
  }
  deriving (Show)

instance NFData Options where
    rnf _ = ()

type ScalaFlags = Options
type ScalaEnv = Options
type ScalaModuleEnv = IORef NameEnv
data ScalaModule = ScalaModule
    { smDefs :: [ScalaDefinition]
    , smNames :: NameEnv
    }
    deriving (Show)
type ScalaDefinition = ScalaExpr

{- Backend contains implementations of hooks called around compilation of Agda code -}
scalaBackend' :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend' =
    BackendBase.Backend'
        { backendName = T.pack "agda2scala"
        , backendVersion = scalaBackendVersion
        , backendInteractTop = Nothing
        , backendInteractHole = Nothing
        , options = defaultOptions
        , commandLineFlags = scalaCmdLineFlags
        , isEnabled = const True
        , preCompile = return
        , compileDef = scalaCompileDef -- Agda definitions => ScalaExpr (Scala AST)
        , postCompile = scalaPostCompile
        , preModule = scalaPreModule
        , postModule = scalaPostModule -- render a whole module to Scala source code as strubg and write it to a file
        , scopeCheckingSuffices = False
        , mayEraseType = const $ return True
        }

---------------------------------------------
-- options

scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags =
    [ Option
        ['o']
        ["out-dir"]
        (ReqArg outDirOpt "DIR")
        "Write output files to DIR. (default: project root)"
    , Option
        ['b']
        ["scala-dialect"]
        (ReqArg scalaDialectOpt "scalaDialect")
        "Write output files using Scala2 or Scala3 dialect. (default: Scala3)"
    , Option
        []
        ["test-out-dir"]
        (ReqArg testOutDirOpt "DIR")
        "Directory for generated Scala property tests"
    ]

outDirOpt :: (Monad m) => FilePath -> Options -> m Options
outDirOpt dir opts = return opts{optOutDir = Just dir}

scalaDialectOpt :: (Monad m) => String -> Options -> m Options
scalaDialectOpt sVer opts = return opts{scalaDialect = Just sVer}

testOutDirOpt :: Monad m => FilePath -> Options -> m Options
testOutDirOpt dir opts = pure opts { optTestOutDir = Just dir }

---------------------------------------------
-- compile into Scala Expr

scalaCompileDef ::
    ScalaEnv ->
    ScalaModuleEnv ->
    IsMain ->
    Definition ->
    TCM ScalaDefinition
scalaCompileDef _env modEnv _isMain def@Defn{defName = qn} =
    withCurrentModule (qnameModule qn) $ do
        modulePragma <- lookupScalaPragmaForDefinition def
        moduleDebugPragma <- lookupScalaDebugPragma qn
        case moduleDebugPragma of
            Nothing -> pure ()
            Just _ -> liftIO $ do
                putStrLn "===== AGDA2SCALA DEBUG ====="
                putStrLn ("QName: " <> show qn)
                putStrLn "defType:"
                print (defType def)
                putStrLn "theDef:"
                print (theDef def)
                putStrLn "===== END DEBUG ====="
        case modulePragma of
            Nothing -> pure (noPragmaResult def)
            Just pragma -> do
                e <- compileDefinition def pragma
                expr <- either compileFailure pure (checkCompile (prettyShow qn) e)
                case expr of
                    SeSum parent _tyParams ctors -> do
                        -- record ctor->parent mapping for later functions
                        liftIO $ atomicModifyIORef' modEnv $ \ne ->
                            let ne' = registerCtors parent ctors ne
                             in (ne', ())
                        pure expr
                    -- register nullary definitions before qualifying their bodies
                    SeFun fName args scheme body -> do
                        ne <- liftIO $ atomicModifyIORef' modEnv $ \current ->
                            let updated =
                                  case args of
                                    [] -> registerNullaryTerm fName current
                                    _  -> current
                             in (updated, updated)
                        case lowerRecordConstruction ne (ssType scheme) body of
                          Left err -> compileFailure (compileErrorMessage (prettyShow qn) err)
                          Right body' -> pure (SeFun fName args scheme body')
                    -- register the record constructor
                    SeProd parent _tyParams _fields -> do
                        case theDef def of
                            RecordDefn (RecordData{_recConHead = conHead}) ->
                                liftIO $ atomicModifyIORef' modEnv $ \ne ->
                                    let recordCtor = ctorName defaultNamePolicy (fromQName (conName conHead))
                                        ne' = registerRecordCtor parent recordCtor ne
                                     in (ne', ())
                            _ -> pure ()
                        pure expr
                    SeTrait parent tyParams fields -> do
                      case theDef def of
                        RecordDefn (RecordData{_recConHead = conHead}) ->
                          liftIO $ atomicModifyIORef' modEnv $ \ne ->
                            let recordCtor = ctorName defaultNamePolicy (fromQName (conName conHead))
                                ne' = registerTraitCtor parent recordCtor (map stbName tyParams) fields ne
                            in (ne', ())
                        _ -> pure ()
                      pure expr
                    _ -> pure expr

lookupScalaPragmaForDefinition
  :: Definition
  -> TCM (Maybe CompilerPragma)
lookupScalaPragmaForDefinition def@Defn{defName = qn} = do
    directPragma <- lookupScalaPragma qn
    case directPragma of
        Just pragma -> pure (Just pragma)
        Nothing -> case theDef def of
          Function{funWith = Just parentFunction} -> lookupScalaPragma parentFunction
          _ -> pure Nothing

checkCompile :: String -> Either CompileError a -> Either String a
checkCompile name = either (Left . compileErrorMessage name . renderCompileError) Right

renderCompileError :: CompileError -> String
renderCompileError (UnsupportedDefinition _) = "UnsupportedDefinition (expected data, record, or function)"
renderCompileError err = show err

compileErrorMessage :: String -> String -> String
compileErrorMessage name err = "agda2scala: failed to compile " <> name <> ": " <> err

compileFailure :: String -> TCM a
compileFailure = liftIO . ioError . userError

lowerRecordConstruction :: NameEnv
  -> ScalaType
  -> ScalaTerm
  -> Either String ScalaTerm
lowerRecordConstruction ne resultType body =
  case body of
    STeApp (STeVar constructor) values | Just info <- lookupTraitCtor constructor ne ->
      constructTrait info values
    _ -> Right (qualifyTermWithEnv ne body)
  where
    constructTrait info values = do
      actualTypes <-
        case resultType of
          STyName name | name == tcParent info -> Right []
          STyApp name args | name == tcParent info -> Right args
          _ -> Left "Record constructor result type mismatch"
      let params = tcTyParams info
          fields = tcFields info
      if length params /= length actualTypes
        then Left "Record type parameter arity mismatch"
        else if length fields /= length values
          then Left "Record field arity mismatch"
          else do
            let substitutions = zip params actualTypes
            members <-
              traverse
                (uncurry (lowerImplementation substitutions))
                (zip fields values)
            case traverse valueMember members of
              Just ordinary ->
                -- Ordinary records
                Right (STeNew resultType ordinary)
              Nothing ->
                -- Records containing polymorphic methods
                Right (STeNewWithTypes resultType members)

    valueMember :: ScalaInstanceMember -> Maybe (SeVar, ScalaTerm)
    valueMember member =
      case member of
        InstanceValue field ibody -> Just (field, ibody)
        _ -> Nothing

    lowerImplementation
      :: [(String, ScalaType)]
      -> ScalaMember
      -> ScalaTerm
      -> Either String ScalaInstanceMember
    lowerImplementation substitutions member value =
      case member of
        MemberValue (SeVar name ty) ->
          Right $
            InstanceValue
              (SeVar name (substituteType substitutions ty))
              (qualifyTermWithEnv ne value)
        MemberType name ->
          Left $
            "Abstract type member implementation is not supported: "
              <> name
        MemberMethod name tyParams args result ->
          case value of
            STeVar function ->
              let instantiate (SeVar argName ty) =
                    SeVar argName (substituteType substitutions ty)
                  methodArgs = map instantiate args
                  callArgs =
                    [ STeVar argName
                    | SeVar argName _ <- methodArgs
                    ]
                  ibody = STeApp (STeVar function) callArgs
              in Right $
                   InstanceMethod
                     name
                     tyParams
                     methodArgs
                     (substituteType substitutions result)
                     (qualifyTermWithEnv ne ibody)
            _ -> Left $ "Unsupported polymorphic method implementation: " <> name

qualifyTermWithEnv :: NameEnv -> ScalaTerm -> ScalaTerm
qualifyTermWithEnv ne = go
  where
    go (STeVar n)
      | isNullaryTerm n ne = STeApp (STeVar n) []
      | otherwise = STeVar (qualifyCtor n)
    go (STeSelect target field) =
      STeSelect (go target) field
    go (STeApp f xs)    = STeApp (go f) (map go xs)
    go (STeLam ns body) = STeLam ns (go body)
    go (STeIf cond thenBranch elseBranch) =
      STeIf (go cond) (go thenBranch) (go elseBranch)
    go (STeBinOp lhs op rhs) =
      STeBinOp (go lhs) op (go rhs)
    go (STeLitInt i)    = STeLitInt i
    go (STeLitBool b)   = STeLitBool b
    go (STeLitString s) = STeLitString s
    go (STeError e)     = STeError e
    go (STeMatch scrut alts) =
      STeMatch
        (go scrut)
        [ (goPat pat, go rhs) | (pat, rhs) <- alts ]
    go (STeNew ty members) =
      STeNew ty [ (field, go value) | (field, value) <- members ]
    go (STeNewWithTypes ty members) =
      STeNewWithTypes ty (map qualifyMember members)
    goPat SPWild          = SPWild
    goPat (SPVar n)       = SPVar n
    goPat (SPCtor n args) = SPCtor (qualifyCtor n) (map goPat args)
    goPat (SPLitInt n)    = SPLitInt n
    goPat (SPLitBool b)   = SPLitBool b
    goPat (SPLitString s) = SPLitString s
    goPat (SPCons headPat tailPat) = SPCons (goPat headPat) (goPat tailPat)
    qualifyCtor n =
      case lookupRecordCtor n ne of
        Just parent -> parent
        Nothing ->
          case lookupCtorOwner n ne of
            Just parent -> parent <> "." <> n
            Nothing     -> n
    qualifyMember member =
      case member of
        InstanceType name ty ->
          InstanceType name ty
        InstanceValue field body ->
          InstanceValue field (go body)
        InstanceMethod name tyParams args result body ->
          InstanceMethod
            name
            tyParams
            args
            result
            (go body)

---------------------------------------------
-- pragmas specific for agda2scala

lookupScalaPragma :: QName -> TCM (Maybe CompilerPragma)
lookupScalaPragma name = getUniqueCompilerPragma pragmaTag name

pragmaTag :: T.Text
pragmaTag = T.pack "AGDA2SCALA"

pragmaDebug :: T.Text
pragmaDebug = T.pack "AGDA2SCALA_DEBUG"

lookupScalaDebugPragma :: QName -> TCM (Maybe CompilerPragma)
lookupScalaDebugPragma qn = getUniqueCompilerPragma pragmaDebug qn

noPragmaResult :: Definition -> ScalaDefinition
-- noPragmaResult Defn{defName = defName} = SeUnhandled (show defName) "No AGDA2SCALA pragma" -- TODO filter Unhandled but show in logs
noPragmaResult _def = SeUnhandled "" ""

---------------------------------------------
-- output Scala code in scalaPostModule

scalaPostModule :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [ScalaDefinition]
  -> TCM ScalaModule
scalaPostModule env modEnv _isMain mName cdefs = do
  nameEnv <- liftIO $ readIORef modEnv
  outDir <- compileDir
  let fileName = scalaFileName mName
      sourceDir = fromMaybe outDir (optOutDir env)
      sourcePath = sourceDir </> fileName
      properties = [ law | SeProperty law <- cdefs ]
      scalaExprs = compileModule mName cdefs
      testTermPrinter =
        case scalaDialect env of
          Just "Scala2" -> printScala2Term
          Just "scala2" -> printScala2Term
          _             -> printScala3Term
      errors = unhandledErrors cdefs
  unless (null errors) (compileFailure (unlines errors))
  when (shouldWriteModule cdefs) $ liftIO $ do
    createDirectoryIfMissing True (takeDirectory sourcePath)
    writeFile sourcePath (selectPrinter env scalaExprs)
  unless (null properties) $
    case optTestOutDir env of
      Nothing -> liftIO $ ioError $ userError "Properties require --test-out-dir"
      Just testDir -> liftIO $ do
        let testFile = dropExtension fileName <> "PropertySpec.scala"
            testPath = testDir </> testFile
            testSource =
              printPropertySuite
                testTermPrinter
                (moduleName mName)
                properties
        createDirectoryIfMissing True (takeDirectory testPath)
        writeFile testPath testSource
  pure ScalaModule
    { smDefs = cdefs
    , smNames = nameEnv
    }

selectPrinter :: Options -> (ScalaExpr -> String)
selectPrinter env = case scalaDialect env of
  Just "Scala3" -> printScala3
  Just "scala3" -> printScala3
  Just "Scala2" -> printScala2
  Just "scala2" -> printScala2
  _             -> printScala3

shouldWriteModule :: [ScalaDefinition] -> Bool
shouldWriteModule defs = not (all unHandled defs)

unhandledErrors :: [ScalaDefinition] -> [String]
unhandledErrors defs =
  [ compileErrorMessage name err
  | SeUnhandled name err <- defs
  , not (null name && null err)
  ]

scalaFileName :: TopLevelModuleName -> FilePath
scalaFileName mName = moduleNameToFileName mName "scala"

compileModule :: TopLevelModuleName -> [ScalaDefinition] -> ScalaDefinition
compileModule mName cdefs = SePackage (moduleName mName) cdefs

moduleName :: TopLevelModuleName -> [String]
moduleName n = Nel.toList (fmap T.unpack (moduleNameParts n))

compileLog :: String -> TCM ()
compileLog msg = liftIO $ putStrLn msg

---------------------------------------------
-- other callbacks

scalaPostCompile ::
    ScalaEnv ->
    IsMain ->
    Map TopLevelModuleName ScalaModule ->
    TCM ()
scalaPostCompile _ _ _ = return ()

scalaPreModule ::
    ScalaEnv ->
    IsMain ->
    TopLevelModuleName ->
    Maybe FilePath ->
    TCM (Recompile ScalaModuleEnv ScalaModule)
scalaPreModule _ _ _ _ = do
    menv <- liftIO initModuleEnv
    pure $ Recompile menv

scalaBackendVersion :: Maybe T.Text
scalaBackendVersion = Just $ T.pack $ showVersion version

defaultOptions :: ScalaFlags
defaultOptions = Options
  { optOutDir = Nothing
  , optTestOutDir = Nothing
  , scalaDialect = Nothing
  }

initModuleEnv :: IO ScalaModuleEnv
initModuleEnv = newIORef emptyNameEnv
