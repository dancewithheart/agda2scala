module Main where

import Hedgehog (Group, checkParallel)
import qualified CompileProps
import qualified Compile.TermsProps
import qualified Compile.TypesProps
import Lower.IRToScalaProps (iRToScalaProps)
import Name.NameEnvProps (nameEnvProps)
import qualified Name.NamePolicyProps
import qualified Render.PrintProps

main :: IO ()
main = do
    ok1 <- checkParallel nameEnvProps
    ok2 <- checkParallel Render.PrintProps.tests
    ok3 <- checkParallel CompileProps.tests
    ok4 <- checkParallel iRToScalaProps
    ok5 <- checkGroups Name.NamePolicyProps.tests
    ok6 <- checkParallel Compile.TermsProps.tests
    ok7 <- checkParallel Compile.TypesProps.tests
    if ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 then pure () else fail "Hedgehog tests failed"

checkGroups :: [Group] -> IO Bool
checkGroups = fmap and . traverse checkParallel
