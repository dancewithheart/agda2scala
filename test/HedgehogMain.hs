module Main where

import qualified AgdaToScalaExprProps
import Hedgehog (Group, checkParallel)
import qualified NameEnvProps
import qualified NamePolicyProps
import qualified PrintProps
import qualified IRToScalaProps

main :: IO ()
main = do
    ok1 <- checkParallel NameEnvProps.tests
    ok2 <- checkParallel PrintProps.tests
    ok3 <- checkParallel AgdaToScalaExprProps.tests
    ok4 <- checkGroups NamePolicyProps.tests
    ok5 <- checkParallel IRToScalaProps.tests
    if ok1 && ok2 && ok3 && ok4 && ok5 then pure () else fail "Hedgehog tests failed"

checkGroups :: [Group] -> IO Bool
checkGroups = fmap and . traverse checkParallel
