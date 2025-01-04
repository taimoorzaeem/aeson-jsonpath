{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Types
  ( JSPQuery (..)
  , JSPSegment (..)
  , JSPChildSegment (..)
  , JSPDescSegment (..)
  , JSPSelector (..)
  , JSPWildcardT (..)
  )
  where

import Data.Text                     (Text)
import Language.Haskell.TH.Syntax    (Lift (..))

import Prelude

newtype JSPQuery
  = JSPRoot [JSPSegment]
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-segments-2
data JSPSegment
  = JSPChildSeg JSPChildSegment
  | JSPDescSeg JSPDescSegment
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-child-segment
data JSPChildSegment
  = JSPChildBracketed [JSPSelector]
  | JSPChildMemberNameSH JSPNameSelector
  | JSPChildWildSeg JSPWildcardT
  deriving (Eq, Show, Lift)


-- https://www.rfc-editor.org/rfc/rfc9535#name-descendant-segment
data JSPDescSegment
  = JSPDescBracketed [JSPSelector]
  | JSPDescMemberNameSH JSPNameSelector
  | JSPDescWildSeg JSPWildcardT
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-selectors-2
data JSPSelector
  = JSPNameSel JSPNameSelector
  | JSPIndexSel JSPIndexSelector
  | JSPSliceSel JSPSliceSelector
  | JSPWildSel JSPWildcardT
  deriving (Eq, Show, Lift)

data JSPWildcardT = JSPWildcard
  deriving (Eq, Show, Lift)

type JSPNameSelector = Text

type JSPIndexSelector = Int

type JSPSliceSelector = (Maybe Int, Maybe Int, Int)
