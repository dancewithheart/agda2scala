module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (
    Test (..),
    failures,
    runTestTT,
 )

import NameEnvTest (nameEnvTests)
import PrintScala2Test (printScala2Tests)
import PrintScala3Test (printScala3Tests)
import ScalaBackendTest (backendTests)

allTests :: Test
allTests =
    TestList
        [ backendTests
        , printScala2Tests
        , printScala3Tests
        , nameEnvTests
        ]

main :: IO ()
main = do
    result <- runTestTT allTests
    if (failures result) > 0 then exitFailure else exitSuccess
