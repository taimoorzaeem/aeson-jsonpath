{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Query.Types
  (Query (..)
  , QueryType (..)
  , Segment (..)
  , QuerySegment (..)
  , SegmentType (..)
  , Selector (..)
  , LogicalOrExpr (..)
  , LogicalAndExpr (..)
  , BasicExpr (..)
  , TestExpr
  , ComparisonExpr (..)
  , ComparisonOp (..)
  , Comparable(..)
  , SingularQuery (..)
  , SingularQueryType (..)
  , SingularQuerySegment (..)
  )
  where

import Data.Text                   (Text)
import Data.Scientific             (Scientific)
import Language.Haskell.TH.Syntax  (Lift)

import Prelude

data QueryType 
  = Root 
  | Current
  deriving (Eq, Show, Lift)

data Query = Query
  { queryType     :: QueryType
  , querySegments :: [QuerySegment]
  } deriving (Eq, Show, Lift)

data SegmentType
  = Child
  | Descendant
  deriving (Eq, Show, Lift)

data QuerySegment = QuerySegment
  { segmentType :: SegmentType
  , segment     :: Segment
  } deriving (Eq, Show, Lift)

data Segment
  = Bracketed [Selector]
  | Dotted Text
  | WildcardSegment
  deriving (Eq, Show, Lift)

data Selector
  = Name Text
  | Index Int
  | ArraySlice Slice
  | Filter LogicalOrExpr
  | WildcardSelector
  deriving (Eq, Show, Lift)

type Slice = (Maybe Int, Maybe Int, Int)

newtype LogicalOrExpr
  = LogicalOr [LogicalAndExpr]
  deriving (Eq, Show, Lift)

newtype LogicalAndExpr
  = LogicalAnd [BasicExpr]
  deriving (Eq, Show, Lift)

data BasicExpr
  = Paren LogicalOrExpr -- ( expr )
  | NotParen LogicalOrExpr -- not (expr)
  | Test TestExpr -- expr
  | NotTest TestExpr -- not expr
  | Comparison ComparisonExpr
  deriving (Eq, Show, Lift)

type TestExpr = Query

data ComparisonExpr
  = Comp Comparable ComparisonOp Comparable
  deriving (Eq, Show, Lift)

data ComparisonOp
  = Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Equal
  | NotEqual
  deriving (Eq, Show, Lift)

data Comparable
  = CompLitString Text
  | CompLitNum Scientific
  | CompSQ SingularQuery
  deriving (Eq, Show, Lift)

data SingularQueryType = RootSQ | CurrentSQ
  deriving (Eq, Show, Lift)

data SingularQuery = SingularQuery
  { singularQueryType     :: SingularQueryType
  , singularQuerySegments :: [SingularQuerySegment]
  } deriving (Eq, Show, Lift)

data SingularQuerySegment
  = NameSQSeg Text
  | IndexSQSeg Int
  deriving (Eq, Show, Lift)
