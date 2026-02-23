module Main where

import Hedgehog (Group(..), checkParallel)
import qualified NameEnvProps
import qualified PrintProps

main :: IO ()
main = do
  ok1 <- checkParallel NameEnvProps.tests
  ok2 <- checkParallel PrintProps.tests
  if ok1 && ok2
  then pure ()
  else fail "Hedgehog tests failed"
