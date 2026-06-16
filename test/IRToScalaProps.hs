{-# LANGUAGE OverloadedStrings #-}

module IRToScalaProps (tests) where

import Hedgehog
  ( Group(..)
  , Property
  , annotateShow
  , assert
  , forAll
  , property
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Agda.Compiler.Scala.IR.AgdaIR
  ( AgdaData(..)
  , AgdaDecl(..)
  , AgdaRecord(..)
  )
import Agda.Compiler.Scala.Lower.IRToScala ( toScalaExpr )
import Agda.Compiler.Scala.Name.NamePolicy ( defaultNamePolicy )
import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr(..)
  , ScalaType(..)
  , SeVar(..)
  )

tests :: Group
tests =
  Group "IRToScala"
    [ ("data lowering preserves constructor arity", prop_dataCtorArityPreserved)
    , ("data lowering applies constructor NamePolicy", prop_dataCtorNamePolicy)
    , ("record lowering preserves field count", prop_recordFieldCountPreserved)
    ]

prop_dataCtorArityPreserved :: Property
prop_dataCtorArityPreserved = property $ do
  arity <- forAll (Gen.int (Range.linear 0 8))

  let ctor =
        ScalaCtor
          { scName = "Mk"
          , scArgs = replicate arity (STyName "A")
          }

      decl =
        DData
          AgdaData
            { adName = "Box"
            , adTyParams = ["A"]
            , adCtors = [ctor]
            }

  case toScalaExpr defaultNamePolicy decl of
    SeSum _ _ [ScalaCtor _ args] ->
      length args === arity
    other -> do
      annotateShow other
      assert False

prop_dataCtorNamePolicy :: Property
prop_dataCtorNamePolicy = property $ do
  let decl =
        DData
          AgdaData
            { adName = "List"
            , adTyParams = ["A"]
            , adCtors =
                [ ScalaCtor
                    { scName = "[]"
                    , scArgs = []
                    }
                ]
            }

  case toScalaExpr defaultNamePolicy decl of
    SeSum _ _ [ScalaCtor ctorName _] ->
      ctorName === "Nil"
    other -> do
      annotateShow other
      assert False

prop_recordFieldCountPreserved :: Property
prop_recordFieldCountPreserved = property $ do
  fieldCount <- forAll (Gen.int (Range.linear 0 8))

  let fields =
        [ SeVar ("x" <> show i) (STyName "A")
        | i <- [0 .. fieldCount - 1]
        ]

      decl =
        DRecord
          AgdaRecord
            { arName = "Rec"
            , arTyParams = []
            , arFields = fields
            }

  case toScalaExpr defaultNamePolicy decl of
    SeProd _ _ vars ->
      length vars === fieldCount
    other -> do
      annotateShow other
      assert False
