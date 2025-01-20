{-# LANGUAGE DeriveLift #-}
{- |
Module      : Data.Aeson.JSONPath.Types.Segment
Description : TODO:
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
module Data.Aeson.JSONPath.Types.Segment
  (Segment (..)
  , QuerySegment (..)
  , SegmentType (..)
  )
  where

import Data.Aeson.JSONPath.Types.Selector (Selector (..))
import Data.Text                          (Text)
import Language.Haskell.TH.Syntax         (Lift)

import Prelude

-- |
data SegmentType
  = Child
  | Descendant
  deriving (Eq, Show, Lift)

-- |
data QuerySegment a = QuerySegment
  { segmentType :: SegmentType
  , segment     :: Segment a
  } deriving (Eq, Show, Lift)

-- |
data Segment a
  = Bracketed [Selector a]
  | Dotted Text
  | WildcardSegment
  deriving (Eq, Show, Lift)
