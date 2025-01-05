{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Types
  ( JSPQuery (..)
  , JSPSegment (..)
  , JSPChildSegment (..)
  , JSPDescSegment (..)
  , JSPSelector (..)
  , JSPWildcardT (..)
  , JSPLogicalExpr (..)
  , JSPLogicalAndExpr (..)
  , JSPBasicExpr (..)
  , JSPParenExpr (..)
  , JSPTestExpr (..)
  , JSPFilterQuery (..)
  , JSPComparisonExpr (..)
  , JSPComparable (..)
  , JSPComparisonOp (..)
  , JSPSingularQuery (..)
  , JSPSingleQSegment (..)
  )
  where


import Data.Scientific               (Scientific)
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
  | JSPFilterSel JSPLogicalExpr
  deriving (Eq, Show, Lift)

data JSPWildcardT = JSPWildcard
  deriving (Eq, Show, Lift)

type JSPNameSelector = Text

type JSPIndexSelector = Int

type JSPSliceSelector = (Maybe Int, Maybe Int, Int)

-- https://www.rfc-editor.org/rfc/rfc9535#section-2.3.5.1
newtype JSPLogicalExpr
  = JSPLogicalOr [JSPLogicalAndExpr]
  deriving (Eq, Show, Lift)

newtype JSPLogicalAndExpr
  = JSPLogicalAnd [JSPBasicExpr]
  deriving (Eq, Show, Lift)

data JSPBasicExpr
  = JSPParen JSPParenExpr
  | JSPTest JSPTestExpr
  | JSPComparison JSPComparisonExpr
  deriving (Eq, Show, Lift)

data JSPParenExpr
  = JSPParenTrue JSPLogicalExpr -- ( expr )
  | JSPParenFalse JSPLogicalExpr -- not ( expr )
  deriving (Eq, Show, Lift)

data JSPTestExpr
  = JSPTestTrue JSPFilterQuery -- filter-query
  | JSPTestFalse JSPFilterQuery -- not filter-query
  deriving (Eq, Show, Lift)

data JSPFilterQuery
  = JSPFilterRelQ [JSPSegment]
  | JSPFilterRootQ [JSPSegment]
  deriving (Eq, Show, Lift)

data JSPComparisonExpr
  = JSPComp JSPComparable JSPComparisonOp JSPComparable
  deriving (Eq, Show, Lift)

data JSPComparable
  = JSPCompLitString Text
  | JSPCompLitNum Scientific
  | JSPCompSQ JSPSingularQuery
  deriving (Eq, Show, Lift)

data JSPComparisonOp
  = JSPLess
  | JSPLessOrEqual
  | JSPGreater
  | JSPGreaterOrEqual
  | JSPEqual
  | JSPNotEqual
  deriving (Eq, Show, Lift)

data JSPSingularQuery
  = JSPRelSingleQ [JSPSingleQSegment]
  | JSPAbsSingleQ [JSPSingleQSegment]
  deriving (Eq, Show, Lift)

data JSPSingleQSegment
  = JSPSingleQNameSeg Text
  | JSPSingleQIndexSeg Int
  deriving (Eq, Show, Lift)
