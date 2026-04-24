module Main where

import qualified AgdaToScalaExprProps
import Hedgehog (checkParallel)
import qualified NameEnvProps
import qualified PrintProps

main :: IO ()
main = do
    ok1 <- checkParallel NameEnvProps.tests
    ok2 <- checkParallel PrintProps.tests
    ok3 <- checkParallel AgdaToScalaExprProps.tests
    if ok1 && ok2 && ok3 then pure () else fail "Hedgehog tests failed"
