{-# LANGUAGE DeriveLift #-}
{- |
Module      : Data.Aeson.JSONPath.Types.Selector
Description : 
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
module Data.Aeson.JSONPath.Types.Selector
  (Selector (..))
  where

import Data.Aeson.JSONPath.Types.Filter (LogicalOrExpr (..))
import Data.Text                        (Text)
import Language.Haskell.TH.Syntax       (Lift)

import Prelude

-- |
data Selector a
  = Name Text
  | Index Int
  | ArraySlice (Maybe Int, Maybe Int, Int)
  | Filter (LogicalOrExpr a)
  | WildcardSelector
  deriving (Eq, Show, Lift)
