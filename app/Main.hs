module Main (main) where

import Agda.Compiler.Scala.Backend (runScalaBackend)

main :: IO ()
main = runScalaBackend
