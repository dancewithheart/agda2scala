module Main where

import qualified CompileProps
import qualified Compile.TermsProps
import qualified Compile.TypesProps
import Hedgehog (Group, checkParallel)
import qualified Name.NameEnvProps
import qualified Name.NamePolicyProps
import qualified Render.PrintProps
import qualified IRToScalaProps

main :: IO ()
main = do
    ok1 <- checkParallel Name.NameEnvProps.tests
    ok2 <- checkParallel Render.PrintProps.tests
    ok3 <- checkParallel CompileProps.tests
    ok4 <- checkGroups Name.NamePolicyProps.tests
    ok5 <- checkParallel IRToScalaProps.tests
    ok6 <- checkParallel Compile.TermsProps.tests
    ok7 <- checkParallel Compile.TypesProps.tests
    if ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 then pure () else fail "Hedgehog tests failed"

checkGroups :: [Group] -> IO Bool
checkGroups = fmap and . traverse checkParallel
