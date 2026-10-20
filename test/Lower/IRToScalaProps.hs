{-# LANGUAGE OverloadedStrings #-}

module Lower.IRToScalaProps (iRToScalaProps) where

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
  , ScalaMember(..)
  , ScalaType(..)
  , ScalaTypeBinder(..)
  , SeVar(..)
  )

iRToScalaProps :: Group
iRToScalaProps =
  Group "IRToScala"
    [ ("data lowering preserves constructor arity", prop_dataCtorArityPreserved)
    , ("data lowering applies constructor NamePolicy", prop_dataCtorNamePolicy)
    , ("record lowering preserves field count", prop_recordFieldCountPreserved)
    , ( "operation records lower to Scala traits"
      , prop_operationRecordLowersToTrait
      )
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
            , adTyParams = [ScalaTypeBinder "A" 0]
            , adCtors = [ctor]
            }
  case toScalaExpr defaultNamePolicy decl of
    SeSum _ _ [ScalaCtor _ args] -> length args === arity
    other -> do
      annotateShow other
      assert False

prop_dataCtorNamePolicy :: Property
prop_dataCtorNamePolicy = property $ do
  let decl =
        DData
          AgdaData
            { adName = "List"
            , adTyParams = [ScalaTypeBinder "A" 0]
            , adCtors =
                [ ScalaCtor
                    { scName = "[]"
                    , scArgs = []
                    }
                ]
            }

  case toScalaExpr defaultNamePolicy decl of
    SeSum _ _ [ScalaCtor ctorName _] -> ctorName === "Nil"
    other -> do
      annotateShow other
      assert False

prop_recordFieldCountPreserved :: Property
prop_recordFieldCountPreserved = property $ do
  fieldCount <- forAll (Gen.int (Range.linear 0 8))
  let fields = [ MemberValue (SeVar ("x" <> show i) (STyName "A")) | i <- [0 .. fieldCount - 1] ]
      decl =
        DRecord
          AgdaRecord
            { arName = "Rec"
            , arTyParams = []
            , arFields = fields
            }

  case toScalaExpr defaultNamePolicy decl of
    SeProd _ _ vars -> length vars === fieldCount
    other -> do
      annotateShow other
      assert False

prop_operationRecordLowersToTrait :: Property
prop_operationRecordLowersToTrait = property $ do
  let a = STyVar "A"
      fields =
        [ MemberValue (SeVar "empty" a)
        , MemberValue
            (SeVar "combine" (STyFun a (STyFun a a)))
        ]
      decl =
        DRecord
          AgdaRecord
            { arName = "Monoid"
            , arTyParams = [ScalaTypeBinder "A" 0]
            , arFields = fields
            }
  toScalaExpr defaultNamePolicy decl
    === SeTrait "Monoid" [ScalaTypeBinder "A" 0] fields

  toScalaExpr defaultNamePolicy decl
    === SeTrait "Monoid" [ScalaTypeBinder "A" 0] fields
