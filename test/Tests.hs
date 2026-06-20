module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Name.NameEnvTest
import qualified Name.NamePolicyTest
import qualified Render.PrintScala2Test
import qualified Render.PrintScala3Test
import qualified ScalaBackendTest
import qualified Compile.TypesTest

main :: IO ()
main =
    defaultMain tests

tests :: TestTree
tests =
    testGroup
        "agda2scala unit tests"
        [ Name.NameEnvTest.tests
        , Name.NamePolicyTest.tests
        , Render.PrintScala2Test.tests
        , Render.PrintScala3Test.tests
        , ScalaBackendTest.tests
        , Compile.TypesTest.tests
        ]