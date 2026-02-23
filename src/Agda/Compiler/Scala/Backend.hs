module Agda.Compiler.Scala.Backend (
  runScalaBackend
  , scalaBackend
  , scalaBackend'
  , defaultOptions
  , outDirOpt
  , scalaDialectOpt
  , initModuleEnv
  , Options(..)
  , selectPrinter
  , shouldWriteModule
  ) where

import Control.DeepSeq ( NFData(..) )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.List.NonEmpty as Nel
import Data.Maybe ( fromMaybe )
import Data.Map ( Map )
import qualified Data.Text as T
import Data.Version ( showVersion )
import Data.IORef (IORef, newIORef, readIORef)

import Paths_agda2scala ( version )

import Agda.Utils.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )
import Agda.Main ( runAgda )
import Agda.Compiler.Backend
  ( Backend(..)
  , Backend'(..)
  , Recompile(..)
  , IsMain
  , Flag
  , TCM
  , Definition(..)
  , CompilerPragma
  , withCurrentModule
  , getUniqueCompilerPragma
  , backendName
  , backendVersion
  , backendInteractTop
  , backendInteractHole
  , options
  , commandLineFlags
  , isEnabled
  , preCompile
  , compileDef
  , postCompile
  , preModule
  , postModule
  , scopeCheckingSuffices
  , mayEraseType
  )
import Agda.Compiler.Backend -- otherwise GHC complains about Backend and Backend'
import Agda.Interaction.Options ( OptDescr )
import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common ( moduleNameParts )
import Agda.Syntax.Internal ( qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Scala.ScalaExpr ( ScalaName, ScalaTypeScheme, ScalaExpr(..), unHandled )
import Agda.Compiler.Scala.AgdaToScalaExpr ( CompileError, compileDefn )
import Agda.Compiler.Scala.PrintScala2 ( printScala2 )
import Agda.Compiler.Scala.PrintScala3 ( printScala3 )
import Agda.Compiler.Scala.NameEnv (NameEnv, emptyNameEnv)

import Data.IORef (atomicModifyIORef', readIORef)
import Agda.Compiler.Scala.NameEnv (registerCtors, lookupCtorOwner)
import Agda.Compiler.Scala.ScalaExpr (ScalaExpr(..), ScalaTerm(..), ScalaCtor(..))

lowerCompile :: QName -> Either CompileError ScalaExpr -> ScalaExpr
lowerCompile qn = either (\err -> SeUnhandled (show qn) (show err)) id

runScalaBackend :: IO ()
runScalaBackend = runAgda [scalaBackend]

scalaBackend :: Backend
scalaBackend = Backend scalaBackend'

data Options = Options {
  optOutDir :: Maybe FilePath,
  scalaDialect :: Maybe String
} deriving (Show)

instance NFData Options where
  rnf _ = ()

type ScalaFlags = Options
type ScalaEnv = Options
type ScalaModuleEnv = IORef NameEnv
data ScalaModule = ScalaModule
  { smDefs  :: [ScalaDefinition]
  , smNames :: NameEnv
  } deriving (Show)
type ScalaDefinition = ScalaExpr

{- Backend contains implementations of hooks called around compilation of Agda code -}
scalaBackend' :: Backend' ScalaFlags ScalaEnv ScalaModuleEnv ScalaModule ScalaDefinition
scalaBackend' = Backend'
  { backendName           = T.pack "agda2scala"
  , backendVersion        = scalaBackendVersion
  , backendInteractTop    = Nothing
  , backendInteractHole   = Nothing
  , options               = defaultOptions
  , commandLineFlags      = scalaCmdLineFlags
  , isEnabled             = const True
  , preCompile            = return
  , compileDef            = scalaCompileDef -- Agda definitions => ScalaExpr (Scala AST)
  , postCompile           = scalaPostCompile
  , preModule             = scalaPreModule
  , postModule            = scalaPostModule -- render a whole module to Scala source code as strubg and write it to a file
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

scalaBackendVersion :: Maybe T.Text
scalaBackendVersion = Just $ T.pack $ showVersion version

defaultOptions :: ScalaFlags
defaultOptions = Options{ optOutDir = Nothing, scalaDialect = Nothing }

scalaCmdLineFlags :: [OptDescr (Flag ScalaFlags)]
scalaCmdLineFlags = [
  Option
    ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
    "Write output files to DIR. (default: project root)",
  Option
    ['b'] ["scala-dialect"] (ReqArg scalaDialectOpt "scalaDialect")
    "Write output files using Scala2 or Scala3 dialect. (default: Scala2)"
  ]

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt dir opts = return opts{ optOutDir = Just dir }

scalaDialectOpt :: Monad m => String -> Options -> m Options
scalaDialectOpt sVer opts = return opts{ scalaDialect = Just sVer }

scalaCompileDef :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> Definition
  -> TCM ScalaDefinition
scalaCompileDef _env modEnv _isMain def@Defn{defName = qn} =
  withCurrentModule (qnameModule qn) $ do
    modulePragma <- lookupScalaPragma qn
    case modulePragma of
      Nothing     -> pure (noPragmaResult def)
      Just pragma -> do
        e   <- compileDefn def pragma   -- TCM (Either CompileError ScalaExpr)
        let expr = lowerCompile qn e    -- ScalaExpr
        case expr of
          SeSum parent ctors -> do
            -- record ctor->parent mapping for later functions
            liftIO $ atomicModifyIORef' modEnv $ \ne ->
              let ne' = registerCtors parent ctors ne
              in (ne', ())
            pure expr

          SeFun fName args scheme body -> do
            ne <- liftIO $ readIORef modEnv
            let body' = qualifyTermWithEnv ne body
            pure (SeFun fName args scheme body')

          _ ->
            pure expr

qualifyTermWithEnv :: NameEnv -> ScalaTerm -> ScalaTerm
qualifyTermWithEnv ne = go
  where
    go (STeVar n) =
      case lookupCtorOwner n ne of
        Just parent -> STeVar (parent <> "." <> n)
        Nothing     -> STeVar n
    go (STeApp f xs)      = STeApp (go f) (map go xs)
    go (STeLam ns body)   = STeLam ns (go body)
    go (STeLitInt i)      = STeLitInt i
    go (STeLitBool b)     = STeLitBool b
    go (STeLitString s)   = STeLitString s
    go (STeError e)       = STeError e

lookupScalaPragma :: QName -> TCM (Maybe CompilerPragma)
lookupScalaPragma defName = getUniqueCompilerPragma pragmaTag defName

pragmaTag :: T.Text
pragmaTag = T.pack "AGDA2SCALA"

noPragmaResult :: Definition -> ScalaDefinition
--noPragmaResult Defn{defName = defName} = SeUnhandled (show defName) "No AGDA2SCALA pragma" -- TODO filter Unhandled but show in logs
noPragmaResult def = SeUnhandled "" ""

scalaPostCompile :: ScalaEnv
  -> IsMain
  -> Map TopLevelModuleName ScalaModule
  -> TCM ()
scalaPostCompile _ _ _ = return ()

scalaPreModule :: ScalaEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> TCM (Recompile ScalaModuleEnv ScalaModule)
scalaPreModule _ _ _ _ = do
  menv <- liftIO $ initModuleEnv
  pure $ Recompile menv

initModuleEnv :: IO ScalaModuleEnv
initModuleEnv = newIORef emptyNameEnv

scalaPostModule :: ScalaEnv
  -> ScalaModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [ScalaDefinition]
  -> TCM ScalaModule
scalaPostModule env modEnv _isMain mName cdefs = do
  nameEnv <- liftIO $ readIORef modEnv
  outDir <- compileDir
  compileLog $ "compiling " <> mkOutFile outDir
  when (shouldWriteModule cdefs)
    $ liftIO
    $ writeFile (mkOutFile outDir) (fileContent nameEnv)
  pure ScalaModule { smDefs  = cdefs, smNames = nameEnv }
  where
    fileName = scalaFileName mName
    dirName outDir = fromMaybe outDir (optOutDir env)
    mkOutFile outDir = (dirName outDir) <> "/" <> fileName
    scalaExprs = compileModule mName cdefs
    fileContent :: NameEnv -> String
    fileContent _nameEnv = selectPrinter env scalaExprs

selectPrinter :: Options -> (ScalaExpr -> String)
selectPrinter env =
  case scalaDialect env of
    Just "Scala3" -> printScala3
    Just "scala2" -> printScala2
    Nothing       -> printScala2

shouldWriteModule :: [ScalaDefinition] -> Bool
shouldWriteModule defs = not (all unHandled defs)

scalaFileName :: TopLevelModuleName -> FilePath
scalaFileName mName = moduleNameToFileName mName "scala"

compileModule :: TopLevelModuleName -> [ScalaDefinition] -> ScalaDefinition
compileModule mName cdefs = SePackage (moduleName mName) cdefs

moduleName :: TopLevelModuleName -> [String]
moduleName n = Nel.toList (fmap T.unpack (moduleNameParts n)) -- (Nel.last (moduleNameParts n))

compileLog :: String -> TCM ()
compileLog msg = liftIO $ putStrLn msg
