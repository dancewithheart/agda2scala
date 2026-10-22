module ScalaBackendTest (tests) where

import Data.IORef (readIORef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Agda.Compiler.Backend (isEnabled)
import Agda.Compiler.Scala.Backend
    ( Options (..)
    , defaultOptions
    , initModuleEnv
    , lowerRecordConstruction
    , substituteType
    , outDirOpt
    , qualifyTermWithEnv
    , scalaBackend'
    , scalaDialectOpt
    , selectPrinter
    , shouldWriteModule
    )
import Agda.Compiler.Scala.IR.ScalaExpr
    ( ScalaCtor (..)
    , ScalaExpr (..)
    , ScalaMember(..)
    , ScalaPat (..)
    , ScalaTerm (..)
    , ScalaType (..)
    , ScalaTypeBinder (..)
    , ScalaInstanceMember (..)
    , SeVar (..)
    )
import Agda.Compiler.Scala.Name.NameEnv
  ( NameEnv
  , emptyNameEnv
  , registerRecordCtor
  , registerTraitCtor
  )
import Agda.Compiler.Scala.Render.PrintScala3 (printScala3)

tests :: TestTree
tests =
    testGroup
        "Scala backend / options and module output"
        [ testCase "backend is enabled by default" test_isEnabled
        , testCase "--out-dir stores target output directory" test_outDirOpt
        , testCase "--scala-dialect stores selected dialect" test_scalaDialectOpt
        , testCase "initModuleEnv starts with empty NameEnv" test_initModuleEnv
        , testCase "selectPrinter defaults to Scala3" test_selectPrinterDefault
        , testCase "selectPrinter chooses Scala3 when requested" test_selectPrinterScala3
        , testCase "shouldWriteModule skips modules with only empty SeUnhandled definitions" test_shouldWriteModule_allUnhandled
        , testCase "shouldWriteModule writes modules with at least one handled definition" test_shouldWriteModule_someHandled
        , testCase "record constructor patterns use the generated record type" test_qualifyRecordConstructorPattern
        , testCase "substituteType replaces nested type variables" test_substituteType
        , testCase "record construction instantiates field types" test_lowerRecordConstruction
        , testCase "record construction rejects incorrect type arity" test_recordTypeArity
        , testCase "record construction rejects incorrect field arity" test_recordFieldArity
        , testCase "record construction rejects mismatched result type" test_recordResultTypeMismatch
        , testCase "polymorphic method implementations" test_polymorphicMethodImplementation
        ]

test_isEnabled :: IO ()
test_isEnabled =
    assertEqual
        "backend enabled"
        True
        (isEnabled scalaBackend' defaultOptions)

test_outDirOpt :: IO ()
test_outDirOpt = do
    opts <- outDirOpt "scala2/src/main/scala" defaultOptions
    assertEqual
        "output directory"
        (Just "scala2/src/main/scala")
        (optOutDir opts)

test_scalaDialectOpt :: IO ()
test_scalaDialectOpt = do
    opts <- scalaDialectOpt "Scala3" defaultOptions
    assertEqual
        "Scala dialect"
        (Just "Scala3")
        (scalaDialect opts)

test_initModuleEnv :: IO ()
test_initModuleEnv = do
    ref <- initModuleEnv
    nameEnv <- readIORef ref
    assertEqual
        "initial module NameEnv"
        emptyNameEnv
        nameEnv

test_selectPrinterDefault :: IO ()
test_selectPrinterDefault = do
    let printer = selectPrinter defaultOptions
        ast = SePackage ["x"] []
    assertEqual
        "default printer output"
        (printScala3 ast)
        (printer ast)

test_selectPrinterScala3 :: IO ()
test_selectPrinterScala3 = do
    let opts = defaultOptions{scalaDialect = Just "Scala3"}
        printer = selectPrinter opts
        ast = SePackage ["x"] []

    assertEqual
        "Scala3 printer output"
        (printScala3 ast)
        (printer ast)

test_shouldWriteModule_allUnhandled :: IO ()
test_shouldWriteModule_allUnhandled =
    assertEqual
        "module should not be written"
        False
        (shouldWriteModule [SeUnhandled "" ""])

test_shouldWriteModule_someHandled :: IO ()
test_shouldWriteModule_someHandled =
    assertEqual
        "module should be written"
        True
        ( shouldWriteModule
            [ SeUnhandled "" ""
            , SeSum "Rgb" [] [ScalaCtor{scName = "Red", scArgs = []}]
            ]
        )

test_qualifyRecordConstructorPattern :: IO ()
test_qualifyRecordConstructorPattern = do
    let nameEnv = registerRecordCtor "Environment" "newEnv" emptyNameEnv
        input = STeMatch
                (STeVar "environment")
                [ ( SPCtor "newEnv" [SPVar "p"], STeVar "p") ]
        expected = STeMatch
                (STeVar "environment")
                [ ( SPCtor "Environment" [SPVar "p"], STeVar "p") ]
    assertEqual
        "qualified record constructor pattern"
        expected
        (qualifyTermWithEnv nameEnv input)

test_substituteType :: IO ()
test_substituteType =
    assertEqual
        "nested type substitution"
        (STyApp "List"
            [STyFun (STyName "Long") (STyVar "B")])
        (substituteType
            [("A", STyName "Long")]
            (STyApp "List"
                [STyFun (STyVar "A") (STyVar "B")]))

monoidEnv :: NameEnv
monoidEnv = registerTraitCtor
    "Monoid"
    "mkMonoid"
    ["A"]
    [ MemberValue (SeVar "empty" (STyVar "A"))
    , MemberValue
        (SeVar "combine"
          (STyFun
            (STyVar "A")
            (STyFun (STyVar "A") (STyVar "A"))))
    ]
    emptyNameEnv

monoidType :: ScalaType
monoidType = STyApp "Monoid" [STyName "Long"]

monoidValues :: [ScalaTerm]
monoidValues =
    [ STeLitInt 0
    , STeVar "plus"
    ]

test_lowerRecordConstruction :: IO ()
test_lowerRecordConstruction =
    assertEqual
        "record construction"
        (Right
            (STeNew monoidType
                [ (SeVar "empty" (STyName "Long"), STeLitInt 0)
                , ( SeVar "combine"
                        (STyFun (STyName "Long")
                            (STyFun (STyName "Long") (STyName "Long")))
                  , STeVar "plus"
                  )
                ]))
        (lowerRecordConstruction
            monoidEnv
            monoidType
            (STeApp (STeVar "mkMonoid") monoidValues))

test_recordTypeArity :: IO ()
test_recordTypeArity =
    assertEqual
        "incorrect type parameter arity"
        (Left "Record type parameter arity mismatch")
        (lowerRecordConstruction
            monoidEnv
            (STyName "Monoid")
            (STeApp (STeVar "mkMonoid") monoidValues))

test_recordFieldArity :: IO ()
test_recordFieldArity =
    assertEqual
        "incorrect field arity"
        (Left "Record field arity mismatch")
        (lowerRecordConstruction
            monoidEnv
            monoidType
            (STeApp (STeVar "mkMonoid") [STeLitInt 0]))

test_recordResultTypeMismatch :: IO ()
test_recordResultTypeMismatch =
    assertEqual
        "incorrect result type"
        (Left "Record constructor result type mismatch")
        (lowerRecordConstruction
            monoidEnv
            (STyApp "Other" [STyName "Long"])
            (STeApp (STeVar "mkMonoid") monoidValues))

test_polymorphicMethodImplementation :: IO ()
test_polymorphicMethodImplementation =
  assertEqual
    "Functor method delegates to the translated implementation"
    (Right expected)
    (lowerRecordConstruction
      functorEnv
      functorType
      (STeApp (STeVar "mkFunctor") [STeVar "mapMaybe"]))
  where
    functorType = STyApp "Functor" [STyName "Option"]
    tyParams =
      [ ScalaTypeBinder "A" 0
      , ScalaTypeBinder "B" 0
      ]
    inputArgs =
      [ SeVar "f" (STyFun (STyVar "A") (STyVar "B"))
      , SeVar "fa" (STyApp "F" [STyVar "A"])
      ]
    resultType = STyApp "F" [STyVar "B"]
    functorEnv =
      registerTraitCtor
        "Functor"
        "mkFunctor"
        ["F"]
        [MemberMethod "map" tyParams inputArgs resultType]
        emptyNameEnv
    expected =
      STeNewWithTypes functorType
        [ InstanceMethod
            "map"
            tyParams
            [ SeVar "f" (STyFun (STyVar "A") (STyVar "B"))
            , SeVar "fa" (STyApp "Option" [STyVar "A"])
            ]
            (STyApp "Option" [STyVar "B"])
            (STeApp
              (STeVar "mapMaybe")
              [STeVar "f", STeVar "fa"])
        ]
