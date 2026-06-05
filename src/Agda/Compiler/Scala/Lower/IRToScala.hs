{-# LANGUAGE LambdaCase #-}

-- lower AgdaIR to ScalaIR
-- NamePolicy: Agda names -> Scala identifiers
module Agda.Compiler.Scala.Lower.IRToScala
  ( toScalaExpr
  ) where

import Agda.Compiler.Scala.IR.Agda
  ( AgdaData(..)
  , AgdaDecl(..)
  , AgdaFun(..)
  , AgdaRecord(..)
  )
import Agda.Compiler.Scala.NamePolicy
  ( NamePolicy
  , ctorName
  , termName
  , typeName
  )
import Agda.Compiler.Scala.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr(..)
  , ScalaName
  , ScalaTerm(..)
  , SeVar(..)
  )

toScalaExpr :: NamePolicy -> AgdaDecl -> ScalaExpr
toScalaExpr policy =
  \case
    DData d ->
      SeSum
        (typeName policy (adName d))
        (adTyParams d)
        (map (lowerCtor policy) (adCtors d))

    DRecord r ->
      SeProd
        (typeName policy (arName r))
        (arTyParams r)
        (map (lowerField policy) (arFields r))

    DFun f ->
      SeFun
        (termName policy (afName f))
        (map (lowerField policy) (afArgs f))
        (afScheme f)
        (lowerTerm policy (afBody f))

lowerCtor :: NamePolicy -> ScalaCtor -> ScalaCtor
lowerCtor policy (ScalaCtor name args) =
  ScalaCtor
    { scName = ctorName policy name
    , scArgs = args
    }

lowerField :: NamePolicy -> SeVar -> SeVar
lowerField policy (SeVar name ty) =
  SeVar (termName policy name) ty

lowerTerm :: NamePolicy -> ScalaTerm -> ScalaTerm
lowerTerm policy =
  \case
    STeVar name ->
      STeVar (termName policy name)

    STeApp f args ->
      STeApp (lowerTerm policy f) (map (lowerTerm policy) args)

    STeLam names body ->
      STeLam (map (termName policy) names) (lowerTerm policy body)

    STeLitInt n ->
      STeLitInt n

    STeLitBool b ->
      STeLitBool b

    STeLitString s ->
      STeLitString s

    STeError err ->
      STeError err
