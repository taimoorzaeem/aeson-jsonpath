module Data.Aeson.JSONPath
  ( query
  , jsonPath)
  where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Key                as K
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Vector                   as V
import qualified Text.ParserCombinators.Parsec as P

import Data.Aeson.JSONPath.Types    (JSPQuery (..)
                                    , JSPSegment (..)
                                    , JSPChildSegment (..)
                                    , JSPDescSegment (..)
                                    , JSPSelector (..)
                                    , JSPWildcardT (..))
import Data.Aeson.JSONPath.Parser   (pJSPQuery)
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text)
import Data.Vector                  (Vector)
import Language.Haskell.TH.Quote    (QuasiQuoter (..))
import Language.Haskell.TH.Syntax   (lift)

import Prelude


jsonPath :: QuasiQuoter
jsonPath = QuasiQuoter
  { quoteExp = \q -> case P.parse pJSPQuery ("failed to parse query: " <> q) q of
      Left err -> fail $ show err
      Right ex -> lift ex
  , quotePat = error "Error: quotePat"
  , quoteType = error "Error: quoteType"
  , quoteDec = error "Error: quoteDec"
  }


query :: JSPQuery -> JSON.Value -> Vector JSON.Value
query (JSPRoot segs) = traverseJSPSegments segs


traverseJSPSegments :: [JSPSegment] -> JSON.Value -> Vector JSON.Value
traverseJSPSegments xs doc =
  case foldl (flip traverseJSPSegment) doc xs of
    o@(JSON.Object _) -> V.singleton o
    (JSON.Array arr) -> arr
    n@(JSON.Number _) -> V.singleton n
    s@(JSON.String _) -> V.singleton s
    b@(JSON.Bool _) -> V.singleton b
    JSON.Null -> V.singleton JSON.Null


traverseJSPSegment :: JSPSegment -> JSON.Value -> JSON.Value
traverseJSPSegment (JSPChildSeg jspChildSeg) doc = traverseJSPChildSeg jspChildSeg doc
traverseJSPSegment (JSPDescSeg jspDescSeg) doc = traverseJSPDescSeg jspDescSeg doc


traverseJSPChildSeg :: JSPChildSegment -> JSON.Value -> JSON.Value
traverseJSPChildSeg (JSPChildBracketed sels) doc = traverseJSPSelectors sels doc
traverseJSPChildSeg (JSPChildMemberNameSH key) (JSON.Object obj) = fromMaybe emptyJSArray $ KM.lookup (K.fromText key) obj
traverseJSPChildSeg (JSPChildMemberNameSH _) _ = emptyJSArray
traverseJSPChildSeg (JSPChildWildSeg JSPWildcard) doc = doc


traverseJSPDescSeg :: JSPDescSegment -> JSON.Value -> JSON.Value
traverseJSPDescSeg (JSPDescBracketed sels) doc = traverseJSPSelectors sels $ JSON.Array $ allElemsRecursive doc
traverseJSPDescSeg (JSPDescMemberNameSH key) doc = traverseDescMembers key doc
traverseJSPDescSeg (JSPDescWildSeg JSPWildcard) doc = JSON.Array $ allElemsRecursive doc

-- TODO: Clean this super messy code, might require some refactoring
traverseDescMembers :: Text -> JSON.Value -> JSON.Value
traverseDescMembers key (JSON.Object obj) = JSON.Array $ V.concat [
    maybe V.empty V.singleton $ KM.lookup (K.fromText key) obj,
    V.map (traverseDescMembers key) (allElemsRecursive (JSON.Array $ V.fromList $ KM.elems obj))
  ]
traverseDescMembers key ar@(JSON.Array _) = JSON.Array $ V.map (traverseDescMembers key) (allElemsRecursive ar)
traverseDescMembers _ _ = JSON.Array V.empty

traverseJSPSelectors :: [JSPSelector] -> JSON.Value -> JSON.Value
traverseJSPSelectors sels doc = JSON.Array $ V.concat $ map traverse' sels
  where
    traverse' = flip traverseJSPSelector doc

traverseJSPSelector :: JSPSelector -> JSON.Value -> Vector JSON.Value
traverseJSPSelector (JSPNameSel key) (JSON.Object obj) = maybe V.empty V.singleton $ KM.lookup (K.fromText key) obj
traverseJSPSelector (JSPNameSel _) _ = V.empty
traverseJSPSelector (JSPIndexSel idx) (JSON.Array arr) = maybe V.empty V.singleton (if idx >= 0 then (V.!?) arr idx else (V.!?) arr (idx + V.length arr))
traverseJSPSelector (JSPIndexSel _) _ = V.empty
traverseJSPSelector (JSPSliceSel sliceVals) (JSON.Array arr) = traverseJSPSliceSelector sliceVals arr
traverseJSPSelector (JSPSliceSel _) _ = V.empty
traverseJSPSelector (JSPWildSel JSPWildcard) (JSON.Array arr) = arr -- if array return the vector
traverseJSPSelector (JSPWildSel JSPWildcard) doc = V.singleton doc
traverseJSPSelector (JSPFilterSel _) doc = V.singleton doc -- implement later


traverseJSPSliceSelector :: (Maybe Int, Maybe Int, Int) -> JSON.Array -> Vector JSON.Value
traverseJSPSliceSelector (start, end, step) doc = getSlice start end step doc
  where
    -- TODO: Refactor this code to make it more pretty
    len = V.length doc
    normalize i = if i >= 0 then i else len + i

    sliceNormalized arr' (n_start, n_end) isStepNeg =
      let (lower, upper) = if isStepNeg then
            (min (max n_end (-1)) (len-1), min (max n_start (-1)) (len-1))
          else
            (min (max n_start 0) len, min (max n_end 0) len)
      in V.slice lower ((if isStepNeg then 1 else 0)+upper-lower) arr'

    getSlice _ _ 0 _ = V.empty
    getSlice (Just st) (Just en) step' arr =
      filterSlice (sliceNormalized arr (normalize st, normalize en) (step' < 0)) step'
    getSlice (Just st) Nothing step' arr =
      filterSlice (sliceNormalized arr (normalize st, len) (step' < 0)) step'
    getSlice Nothing (Just en) step' arr =
      filterSlice (sliceNormalized arr (0, normalize en) (step' < 0)) step'
    getSlice Nothing Nothing step' arr = filterSlice arr step'

    -- trying to avoid a step loop and keeping it "functional"
    filterSlice slice 1   = slice
    filterSlice slice (-1) = V.reverse slice
    filterSlice slice n = if n < 0 then
          V.ifilter (\i _ -> i `mod` (-n) == 0) $ V.reverse $ V.drop (V.length slice `mod` (-n)) slice
        else
          V.ifilter (\i _ -> i `mod` n == 0) slice

emptyJSArray :: JSON.Value
emptyJSArray = JSON.Array V.empty

allElemsRecursive :: JSON.Value -> Vector JSON.Value
allElemsRecursive (JSON.Object obj) = V.concat [
    V.fromList (KM.elems obj),
    V.concat $ map allElemsRecursive (KM.elems obj)
  ]
allElemsRecursive (JSON.Array arr) = V.concat [
    arr,
    V.concat $ map allElemsRecursive (V.toList arr)
  ]
allElemsRecursive _ = V.empty
