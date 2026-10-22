-- lower AgdaIR to ScalaIR
-- NamePolicy: Agda names -> Scala identifiers
module Agda.Compiler.Scala.Lower.IRToScala
  ( toScalaExpr
  ) where

import Agda.Compiler.Scala.IR.AgdaIR
  ( AgdaData(..)
  , AgdaDecl(..)
  , AgdaFun(..)
  , AgdaRecord(..)
  )
import Agda.Compiler.Scala.Name.NamePolicy
  ( NamePolicy
  , ctorName
  , termName
  , typeName
  )
import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor(..)
  , ScalaExpr(..)
  , ScalaInstanceMember(..)
  , ScalaMember(..)
  , ScalaPat(..)
  , ScalaTerm(..)
  , SeVar(..)
  , ScalaType(..)
  )
import Agda.Compiler.Scala.Lower.Variance ( inferDataTyParams )

toScalaExpr :: NamePolicy -> AgdaDecl -> ScalaExpr
toScalaExpr policy decl = case decl of
  DData d ->
    SeSum
      (typeName policy (adName d))
      (inferDataTyParams (adName d) (adTyParams d) (adCtors d))
      (map (lowerCtor policy) (adCtors d))
  DRecord r ->
    let name = typeName policy (arName r)
        tyParams = arTyParams r
        members = map (lowerMember policy) (arFields r)
    in
      case traverse valueMember members of
        Just values | not (any isOperationMember members) ->
          SeProd name tyParams values
        _ -> SeTrait name tyParams members
  DFun f ->
    SeFun
      (termName policy (afName f))
      (map (lowerField policy) (afArgs f))
      (afScheme f)
      (lowerTerm policy (afBody f))

lowerMember :: NamePolicy -> ScalaMember -> ScalaMember
lowerMember policy member =
  case member of
    MemberType name -> MemberType name
    MemberValue field -> MemberValue (lowerField policy field)
    MemberMethod name tyParams args result ->
      MemberMethod
        (termName policy name)
        tyParams
        (map (lowerField policy) args)
        result

valueMember :: ScalaMember -> Maybe SeVar
valueMember (MemberValue field) = Just field
valueMember _ = Nothing

isOperationMember :: ScalaMember -> Bool
isOperationMember MemberMethod {} = True
isOperationMember (MemberValue (SeVar _ STyFun{})) = True
isOperationMember (MemberType _) = True
isOperationMember _ = False

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
lowerTerm policy term = case term of
    STeVar name         -> STeVar (termName policy name)
    STeApp f args       ->
      STeApp (lowerTerm policy f) (map (lowerTerm policy) args)
    STeLam names body   ->
      STeLam (map (termName policy) names) (lowerTerm policy body)
    STeLitInt n         -> STeLitInt n
    STeLitBool b        -> STeLitBool b
    STeLitString s      -> STeLitString s
    STeError err        -> STeError err
    STeIf cond thenBranch elseBranch ->
      STeIf
        (lowerTerm policy cond)
        (lowerTerm policy thenBranch)
        (lowerTerm policy elseBranch)
    STeBinOp lhs op rhs ->
      STeBinOp
        (lowerTerm policy lhs)
        op
        (lowerTerm policy rhs)
    STeMatch scrut alts ->
      STeMatch
        (lowerTerm policy scrut)
        [ (lowerPat policy pat, lowerTerm policy rhs) | (pat, rhs) <- alts ]
    STeNewWithTypes ty members ->
          STeNewWithTypes ty (map lowerInstance members)
    STeSelect target field ->
      STeSelect
        (lowerTerm policy target)
        (termName policy field)
    STeNew ty members ->
      STeNew ty
        [ (lowerField policy field, lowerTerm policy body)
        | (field, body) <- members
        ]
    where
      lowerInstance :: ScalaInstanceMember -> ScalaInstanceMember
      lowerInstance member =
        case member of
          InstanceType name ty ->
            InstanceType name ty
          InstanceValue field body ->
            InstanceValue
              (lowerField policy field)
              (lowerTerm policy body)
          InstanceMethod name tyParams args result body ->
            InstanceMethod
              (termName policy name)
              tyParams
              (map (lowerField policy) args)
              result
              (lowerTerm policy body)

lowerPat :: NamePolicy -> ScalaPat -> ScalaPat
lowerPat policy pat = case pat of
  SPWild           -> SPWild
  SPVar name       -> SPVar (termName policy name)
  SPCtor name args -> SPCtor (ctorName policy name) (map (lowerPat policy) args)
  SPLitInt n       -> SPLitInt n
  SPLitBool b      -> SPLitBool b
  SPLitString s    -> SPLitString s
  SPCons headPat tailPat ->
    SPCons (lowerPat policy headPat) (lowerPat policy tailPat)
