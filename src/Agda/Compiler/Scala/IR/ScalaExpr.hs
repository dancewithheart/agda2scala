module Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaName
  , ScalaInstanceMember (..)
  , ScalaType (..)
  , ScalaExpr (..)
  , SeVar (..)
  , ScalaMember(..)
  , ScalaPat (..)
  , ScalaProperty(..)
  , ScalaTerm (..)
  , ScalaTypeScheme (..)
  , ScalaCtor (..)
  , ScalaTyParam (..)
  , ScalaVariance (..)
  , ScalaTypeBinder (..)
  , scalaTypeScheme
  , tyParamNames
  , unHandled
) where

type ScalaName = String

data ScalaType
    = STyName ScalaName -- Int, MyType
    | STyVar ScalaName -- A
    | STyFun ScalaType ScalaType -- Int => String
    | STyApp ScalaName [ScalaType] -- Either[A,B], List[A]
    deriving (Eq, Show)

data ScalaTypeScheme = ScalaTypeScheme
    { ssTyParams :: [ScalaTypeBinder] -- [A,B]
    , ssType :: ScalaType -- A => B
    }
    deriving (Eq, Show)

scalaTypeScheme :: ScalaType -> ScalaTypeScheme
scalaTypeScheme st = ScalaTypeScheme{ssTyParams = [], ssType = st}

data ScalaPat
    = SPWild
    | SPVar ScalaName
    | SPCtor ScalaName [ScalaPat]
    | SPCons ScalaPat ScalaPat
    | SPLitInt Int
    | SPLitBool Bool
    | SPLitString String
    deriving (Eq, Show)

data ScalaTerm
  = STeVar ScalaName
  | STeApp ScalaTerm [ScalaTerm]
  | STeSelect ScalaTerm ScalaName
  | STeLam [ScalaName] ScalaTerm
  | STeMatch ScalaTerm [(ScalaPat, ScalaTerm)]
  | STeIf ScalaTerm ScalaTerm ScalaTerm
  | STeBinOp ScalaTerm ScalaName ScalaTerm
  | STeLitInt Int
  | STeLitBool Bool
  | STeLitString String
  | STeError String
  | STeNew ScalaType [(SeVar, ScalaTerm)]
  | STeNewWithTypes ScalaType [ScalaInstanceMember]
  deriving (Eq, Show)

data SeVar = SeVar ScalaName ScalaType
    deriving (Eq, Show)

data ScalaCtor = ScalaCtor
    { scName :: ScalaName
    , scArgs :: [ScalaType]
    }
    deriving (Eq, Show)

data ScalaMember
  = MemberValue SeVar
  | MemberType ScalaName
  | MemberMethod
      ScalaName
      [ScalaTypeBinder]
      [SeVar]
      ScalaType
  deriving (Eq, Show)

data ScalaProperty = ScalaProperty
  { spName  :: ScalaName
  , spArgs  :: [SeVar]
  , spLeft  :: ScalaTerm
  , spRight :: ScalaTerm
  }
  deriving (Eq, Show)

data ScalaInstanceMember
  = InstanceType ScalaName ScalaType
  | InstanceValue SeVar ScalaTerm
  | InstanceMethod
      ScalaName
      [ScalaTypeBinder]
      [SeVar]
      ScalaType
      ScalaTerm -- method body
  deriving (Eq, Show)

data ScalaExpr
    = SePackage [ScalaName] [ScalaExpr]
    | SeSum ScalaName [ScalaTyParam] [ScalaCtor]
    | SeProd ScalaName [ScalaTypeBinder] [SeVar]
    | SeTrait ScalaName [ScalaTypeBinder] [ScalaMember]
    | SeFun ScalaName [SeVar] ScalaTypeScheme ScalaTerm
    | SeProperty ScalaProperty      -- this is not part of Scala language, maybe I should have higher level language ?
    | SeUnhandled ScalaName String
    deriving (Eq, Show)

unHandled :: ScalaExpr -> Bool
unHandled (SeUnhandled _ _) = True
unHandled (SeProperty _) = True
unHandled _ = False

data ScalaVariance
  = Invariant
  | Covariant
  deriving (Eq, Show)

data ScalaTyParam = ScalaTyParam
  { stpBinder :: ScalaTypeBinder
  , stpVariance :: ScalaVariance
  }
  deriving (Eq, Show)

tyParamNames :: [ScalaTyParam] -> [ScalaName]
tyParamNames = map (stbName . stpBinder)

data ScalaTypeBinder = ScalaTypeBinder
  { stbName :: ScalaName
  , stbArity :: Int
  }
  deriving (Eq, Show)
