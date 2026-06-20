module Main where

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
    ok1 <- checkParallel nameEnvProps
    ok2 <- checkParallel printProps
    ok3 <- checkParallel compileProps
    ok4 <- checkParallel iRToScalaProps
    ok5 <- checkGroups namePolicyProps
    ok6 <- checkParallel termsProps
    ok7 <- checkParallel typesProps
    if ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 then pure () else fail "Hedgehog tests failed"

checkGroups :: [Group] -> IO Bool
checkGroups = fmap and . traverse checkParallel
