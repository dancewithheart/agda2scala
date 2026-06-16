module ScalaBackendTest (tests) where

import Data.IORef (readIORef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Agda.Compiler.Backend (isEnabled)
import Agda.Compiler.Scala.Backend
    ( Options (..)
    , defaultOptions
    , initModuleEnv
    , outDirOpt
    , scalaBackend'
    , scalaDialectOpt
    , selectPrinter
    , shouldWriteModule
    )
import Agda.Compiler.Scala.IR.ScalaExpr
    ( ScalaCtor (..)
    , ScalaExpr (..)
    )
import Agda.Compiler.Scala.Name.NameEnv (emptyNameEnv)
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3)

tests :: TestTree
tests =
    testGroup
        "Scala backend / options and module output"
        [ testCase "backend is enabled by default" test_isEnabled
        , testCase "--out-dir stores target output directory" test_outDirOpt
        , testCase "--scala-dialect stores selected dialect" test_scalaDialectOpt
        , testCase "initModuleEnv starts with empty NameEnv" test_initModuleEnv
        , testCase "selectPrinter defaults to Scala2" test_selectPrinterDefault
        , testCase "selectPrinter chooses Scala3 when requested" test_selectPrinterScala3
        , testCase "shouldWriteModule skips modules with only empty SeUnhandled definitions" test_shouldWriteModule_allUnhandled
        , testCase "shouldWriteModule writes modules with at least one handled definition" test_shouldWriteModule_someHandled
        ]

test_isEnabled :: IO ()
test_isEnabled =
    assertEqual
        "backend enabled"
        True
        (isEnabled scalaBackend' defaultOptions)

test_outDirOpt :: IO ()
test_outDirOpt = do
    opts <- outDirOpt "scala2/src/main/scala" defaultOptions
    assertEqual
        "output directory"
        (Just "scala2/src/main/scala")
        (optOutDir opts)

test_scalaDialectOpt :: IO ()
test_scalaDialectOpt = do
    opts <- scalaDialectOpt "Scala3" defaultOptions
    assertEqual
        "Scala dialect"
        (Just "Scala3")
        (scalaDialect opts)

test_initModuleEnv :: IO ()
test_initModuleEnv = do
    ref <- initModuleEnv
    nameEnv <- readIORef ref
    assertEqual
        "initial module NameEnv"
        emptyNameEnv
        nameEnv

test_selectPrinterDefault :: IO ()
test_selectPrinterDefault = do
    let printer = selectPrinter defaultOptions
        ast = SePackage ["x"] []
    assertEqual
        "default printer output"
        (printScala3 ast)
        (printer ast)

test_selectPrinterScala3 :: IO ()
test_selectPrinterScala3 = do
    let opts = defaultOptions{scalaDialect = Just "Scala3"}
        printer = selectPrinter opts
        ast = SePackage ["x"] []

    assertEqual
        "Scala3 printer output"
        (printScala3 ast)
        (printer ast)

test_shouldWriteModule_allUnhandled :: IO ()
test_shouldWriteModule_allUnhandled =
    assertEqual
        "module should not be written"
        False
        (shouldWriteModule [SeUnhandled "" ""])

test_shouldWriteModule_someHandled :: IO ()
test_shouldWriteModule_someHandled =
    assertEqual
        "module should be written"
        True
        ( shouldWriteModule
            [ SeUnhandled "" ""
            , SeSum "Rgb" [] [ScalaCtor{scName = "Red", scArgs = []}]
            ]
        )
