{-# LANGUAGE DeriveLift #-}
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
  , ComparisonExpr (..)
  , ComparisonOp (..)
  , Comparable (..)
  , SingularQueryType (..)
  , SingularQuery (..)
  , SingularQuerySegment (..)
  )
  where

import Data.Text                   (Text)
import Data.Scientific             (Scientific)

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
  | Test a -- query
  | NotTest a -- not query
  | Comparison ComparisonExpr
  deriving (Eq, Show, Lift)

-- |
data ComparisonExpr
  = Comp Comparable ComparisonOp Comparable
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
data Comparable
  = CompLitString Text
  | CompLitNum Scientific
  | CompLitBool Bool
  | CompLitNull
  | CompSQ SingularQuery
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
