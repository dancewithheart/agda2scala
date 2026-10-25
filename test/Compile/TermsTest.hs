module Compile.TermsTest (tests) where

import Data.List (isInfixOf)
import Agda.Syntax.Abstract.Name (QName, mkName_, qualify_)
import Agda.Syntax.Common (ArgInfo, NameId(..), defaultArgInfo, defaultArg, setQuantity, zeroQuantity)
import Agda.Syntax.Internal (Abs(..), Elim'(..), Term(..))
import Agda.Syntax.Literal (Literal(..))
import Agda.Syntax.TopLevelModuleName.Boot (noModuleNameHash)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import Agda.Compiler.Scala.Backend (checkCompile)
import Agda.Compiler.Scala.Compile.Terms (Env(..), compileBodyTerm, envFromArgs)
import Agda.Compiler.Scala.Compile.Types (CompileError(..))
import Agda.Compiler.Scala.IR.ScalaExpr (ScalaTerm(..))

tests :: TestTree
tests = testGroup "Compile.Terms"
  [ testCase "erased proof lambda contributes no runtime function" test_erasedLambda
  , testCase "erased proof lambda retains outer de Bruijn indices" test_erasedLambdaOuterVar
  , testCase "erased proof cannot be used at runtime" test_erasedLambdaReference
  , testCase "explicit lambda lowers to a Scala function" test_runtimeLambda
  , testCase "nested runtime lambdas retain de Bruijn indices" test_nestedLambda
  , testCase "erased binder followed by runtime lambda preserves scope" test_erasedThenRuntimeLambda
  , testCase "explicit erased application arguments disappear" test_erasedApplication
  , testCase "if branches containing erased lambdas lower" test_ifErasedBranches
  , testCase "unsupported terms produce a useful compilation failure" test_compileFailure
  ]

erasedInfo :: ArgInfo
erasedInfo = setQuantity zeroQuantity defaultArgInfo

erased :: Term -> Term
erased body = Lam erasedInfo (Abs "proof" body)

test_erasedLambda :: Assertion
test_erasedLambda =
  compileBodyTerm (Env []) (erased (Lit (LitNat 1))) @?= Right (STeLitInt 1)

test_erasedLambdaOuterVar :: Assertion
test_erasedLambdaOuterVar =
  compileBodyTerm (envFromArgs ["outer"]) (erased (Var 1 [])) @?= Right (STeVar "outer")

test_erasedLambdaReference :: Assertion
test_erasedLambdaReference =
  compileBodyTerm (Env []) (erased (Var 0 [])) @?= Left (ErasedVarReferenced 0)

test_runtimeLambda :: Assertion
test_runtimeLambda =
  compileBodyTerm (Env []) (Lam defaultArgInfo (Abs "x" (Var 0 [])))
    @?= Right (STeLam ["lambdaArg0"] (STeVar "lambdaArg0"))

test_nestedLambda :: Assertion
test_nestedLambda =
  compileBodyTerm (Env []) (Lam defaultArgInfo (Abs "x"
    (Lam defaultArgInfo (Abs "y" (Var 1 [])))))
    @?= Right (STeLam ["lambdaArg0"] (STeLam ["lambdaArg1"] (STeVar "lambdaArg0")))

test_erasedThenRuntimeLambda :: Assertion
test_erasedThenRuntimeLambda =
  compileBodyTerm (envFromArgs ["outer"]) (erased
    (Lam defaultArgInfo (Abs "x" (Var 2 []))))
    @?= Right (STeLam ["lambdaArg0"] (STeVar "outer"))

test_erasedApplication :: Assertion
test_erasedApplication =
  compileBodyTerm (envFromArgs ["x"]) (Def function
    [ Apply (defaultArg (Var 0 []))
    , Apply (setQuantity zeroQuantity (defaultArg (Var 0 [])))
    ]) @?= Right (STeApp (STeVar "f") [STeVar "x"])

test_ifErasedBranches :: Assertion
test_ifErasedBranches =
  compileBodyTerm (envFromArgs ["choice"]) (Def ifName
    [ Apply (defaultArg (Var 0 []))
    , Apply (defaultArg (erased (Lit (LitNat 1))))
    , Apply (defaultArg (erased (Lit (LitNat 0))))
    ]) @?= Right (STeIf (STeVar "choice") (STeLitInt 1) (STeLitInt 0))

test_compileFailure :: Assertion
test_compileFailure =
  case checkCompile "ScalaErasedIf.ifChoice"
         (Left (UnsupportedTerm (erased (Var 0 []))) :: Either CompileError ScalaTerm) of
    Left err -> do
      assertBool "definition named" ("ScalaErasedIf.ifChoice" `isInfixOf` err)
      assertBool "error identified" ("UnsupportedTerm" `isInfixOf` err)
    Right _ -> assertFailure "unsupported term was accepted"

function :: QName
function = qualify_ (mkName_ (NameId 0 noModuleNameHash) ("f" :: String))

ifName :: QName
ifName = qualify_ (mkName_ (NameId 1 noModuleNameHash) ("if_then_else_" :: String))
