{-# LANGUAGE DeriveLift   #-}
{- |
Module      : Data.Aeson.JSONPath.Types.Filter
Description : 
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
module Data.Aeson.JSONPath.Types.Filter
  (LogicalOrExpr (..)
  , LogicalAndExpr (..)
  , BasicExpr (..)
  , TestExpr (..)
  , ComparisonExpr (..)
  , ComparisonOp (..)
  , Comparable (..)
  , Literal (..)
  , SingularQueryType (..)
  , SingularQuery (..)
  , SingularQuerySegment (..)
  , FunctionExpr (..)
  , FunctionName (..)
  , FunctionArg (..)
  , FunctionResult (..)
  )
  where

import Data.Aeson                  (Value)
import Data.Text                   (Text)
import Data.Scientific             (Scientific)
import Data.Vector                 (Vector)
import Language.Haskell.TH.Syntax  (Lift)

import Prelude

-- |
newtype LogicalOrExpr a
  = LogicalOr [LogicalAndExpr a]
  deriving (Eq, Show, Lift)

-- |
newtype LogicalAndExpr a
  = LogicalAnd [BasicExpr a]
  deriving (Eq, Show, Lift)

-- |
data BasicExpr a
  = Paren (LogicalOrExpr a) -- ( expr )
  | NotParen (LogicalOrExpr a) -- not (expr)
  | Test (TestExpr a) -- query
  | NotTest (TestExpr a) -- not query
  | Comparison (ComparisonExpr a)
  deriving (Eq, Show, Lift)

-- |
data TestExpr a
  = FilterQuery a
  | TestFunc (FunctionExpr a)
  deriving (Eq, Show, Lift)

-- |
data ComparisonExpr a
  = Comp (Comparable a) ComparisonOp (Comparable a)
  deriving (Eq, Show, Lift)

-- |
data ComparisonOp
  = Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Equal
  | NotEqual
  deriving (Eq, Show, Lift)

-- |
data Comparable a
  = CompLit Literal
  | CompSQ SingularQuery
  | CompFunc (FunctionExpr a) -- Value Type
  deriving (Eq, Show, Lift)

-- |
data Literal
  = LitString Text
  | LitNum Scientific
  | LitBool Bool
  | LitNull
  deriving (Eq, Show, Lift)

-- |
data SingularQueryType = RootSQ | CurrentSQ
  deriving (Eq, Show, Lift)

-- |
data SingularQuery = SingularQuery
  { singularQueryType     :: SingularQueryType
  , singularQuerySegments :: [SingularQuerySegment]
  } deriving (Eq, Show, Lift)

-- |
data SingularQuerySegment
  = NameSQSeg Text
  | IndexSQSeg Int
  deriving (Eq, Show, Lift)

-- Function Extensions

-- |
data FunctionExpr a = FunctionExpr
  { functionName :: FunctionName
  , functionArgs :: [FunctionArg a]
  }
  deriving (Eq, Show, Lift)

-- |
data FunctionName
  = Length -- only have length function for now
  -- Count
  -- Match
  -- Search
  -- Value
  deriving (Eq, Show, Lift)

-- |
data FunctionArg a
  = ArgLit Literal
  | ArgQuery a
  | ArgLogicExpr (LogicalOrExpr a)
  | ArgFuncExpr (FunctionExpr a)
  deriving (Eq, Show, Lift)

-- |
data FunctionResult
  = NodesType (Vector Value)
  | LogicalType Bool
  | ValueType (Maybe Value)
  deriving Eq
