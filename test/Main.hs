module Main (main) where

import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (
    Test (..),
    cases,
    errors,
    failures,
    runTestTT,
    tried
 )
import Name.NameEnvTest (nameEnvTests)
import Name.NamePolicyTest (namePolicytests)
import Compile.TypesTest (typeTests)
import Render.PrintScala2Test (printScala2Tests)
import Render.PrintScala3Test (printScala3Tests)
import ScalaBackendTest (backendTests)

allTests :: Test
allTests =
    TestList
        [ backendTests
        , printScala2Tests
        , printScala3Tests
        , nameEnvTests
        , namePolicytests
        , typeTests
        ]

main :: IO ()
main = do
  counts <- runTestTT allTests

  putStrLn ""
  putStrLn "━━━ HUnit summary ━━━"
  putStrLn ("cases:    " <> show (cases counts))
  putStrLn ("tried:    " <> show (tried counts))
  putStrLn ("errors:   " <> show (errors counts))
  putStrLn ("failures: " <> show (failures counts))

  when (errors counts + failures counts > 0) exitFailure
