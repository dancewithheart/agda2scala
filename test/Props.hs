module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Hedgehog (Group, checkParallel)

import CompileProps (compileProps)
import Compile.TermsProps (termsProps)
import Compile.TypesProps (typesProps)
import Lower.IRToScalaProps (iRToScalaProps)
import Name.NameEnvProps (nameEnvProps)
import Name.NamePolicyProps (namePolicyProps)
import Render.PrintProps (printProps)

main :: IO ()
main = do
    ok <- checkAllGroups allGroups
    unless ok exitFailure

allGroups :: [Group]
allGroups =
    [ nameEnvProps
    , printProps
    , compileProps
    , iRToScalaProps
    , termsProps
    , typesProps
    ]
        <> namePolicyProps

checkAllGroups :: [Group] -> IO Bool
checkAllGroups groups = and <$> traverse checkParallel groups
