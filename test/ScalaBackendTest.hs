module ScalaBackendTest (backendTests) where

import Data.IORef (readIORef)
import Test.HUnit (Test (..), assertEqual)

import Agda.Compiler.Backend (isEnabled)
import Agda.Compiler.Scala.Backend (
    Options (..),
    defaultOptions,
    initModuleEnv,
    outDirOpt,
    scalaBackend',
    scalaDialectOpt,
    selectPrinter,
    shouldWriteModule,
 )
import Agda.Compiler.Scala.NameEnv (emptyNameEnv)
import Agda.Compiler.Scala.PrintScala2 (printScala2)
import Agda.Compiler.Scala.PrintScala3 (printScala3)
import Agda.Compiler.Scala.ScalaExpr (ScalaCtor (..), ScalaExpr (..))

testIsEnabled :: Test
testIsEnabled =
    TestCase
        (assertEqual "isEnabled" (isEnabled scalaBackend' defaultOptions) True)

-- test option setters

testOutDirOpt :: Test
testOutDirOpt = TestCase $ do
    opts' <- outDirOpt "scala2/src/main/scala" defaultOptions
    assertEqual "outDirOpt sets optOutDir" (optOutDir opts') (Just "scala2/src/main/scala")

testScalaDialectOpt :: Test
testScalaDialectOpt = TestCase $ do
    opts' <- scalaDialectOpt "Scala3" defaultOptions
    assertEqual "scalaDialectOpt sets scalaDialect" (scalaDialect opts') (Just "Scala3")

-- test scalaPreModule initializes NameEnv correctly
testInitModuleEnv :: Test
testInitModuleEnv = TestCase $ do
    ref <- initModuleEnv
    ne <- readIORef ref
    assertEqual "initModuleEnv starts empty" ne emptyNameEnv

-- test dialect chooses printer
testSelectPrinterDefault :: Test
testSelectPrinterDefault = TestCase $ do
    let p = selectPrinter defaultOptions
    -- Compare by applying to a tiny AST; functions can't be compared directly.
    let ast = SePackage ["x"] []
    assertEqual "default is Scala2 printer" (p ast) (printScala2 ast)

testSelectPrinterScala3 :: Test
testSelectPrinterScala3 = TestCase $ do
    let opts = defaultOptions{scalaDialect = Just "Scala3"}
        p = selectPrinter opts
        ast = SePackage ["x"] []
    assertEqual "Scala3 printer chosen" (p ast) (printScala3 ast)

testShouldWriteModule :: Test
testShouldWriteModule = TestCase $ do
    assertEqual
        "all unhandled => don't write"
        False
        (shouldWriteModule [SeUnhandled "" ""])
    assertEqual
        "some handled => write"
        True
        (shouldWriteModule [SeUnhandled "" "", SeSum "Rgb" [] [ScalaCtor{scName = "Red", scArgs = []}]])

backendTests :: Test
backendTests =
    TestList
        [ TestLabel "isEnabled" testIsEnabled
        , TestLabel "--out-dir=scala2/src/main/scala" testOutDirOpt
        , TestLabel "--scala-dialect=Scala3" testScalaDialectOpt
        , TestLabel "initModuleEnv starts empty" testInitModuleEnv
        , TestLabel "default is Scala2 printer" testSelectPrinterDefault
        , TestLabel "Scala3 printer chosen" testSelectPrinterScala3
        , TestLabel "write module when there are unhandled defs" testShouldWriteModule
        ]
