module Data.Aeson.JSONPath
  ( runJSPQuery )
  where

import qualified Data.Aeson                    as JSON
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Aeson.Key                as K
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Vector                   as V

import Data.Aeson.JSONPath.Parser (JSPQuery (..)
                                  , JSPSegment (..)
                                  , JSPChildSegment (..)
                                  , JSPSelector (..)
                                  , JSPWildcardT (..)
                                  , pJSPQuery)
import Protolude


runJSPQuery :: Text -> JSON.Value -> Either P.ParseError JSON.Value
runJSPQuery query document = do
  jspath <- P.parse pJSPQuery ("failed to parse query: " <> toS query) (toS query)
  return $ traverseJSPQuery jspath document


traverseJSPQuery :: JSPQuery -> JSON.Value -> JSON.Value
traverseJSPQuery (JSPRoot segs) doc = traverseJSPSegments segs doc


traverseJSPSegments :: [JSPSegment] -> JSON.Value -> JSON.Value
traverseJSPSegments [] doc = doc
traverseJSPSegments (x:xs) doc = traverseJSPSegments xs (traverseJSPSegment x doc)


traverseJSPSegment :: JSPSegment -> JSON.Value -> JSON.Value
traverseJSPSegment (JSPChildSeg jspChildSeg) doc = traverseJSPChildSeg jspChildSeg doc


traverseJSPChildSeg :: JSPChildSegment -> JSON.Value -> JSON.Value
traverseJSPChildSeg (JSPBracketed sels) doc = traverseJSPSelectors sels doc
traverseJSPChildSeg (JSPMemberNameSH key) (JSON.Object obj) = fromMaybe emptyJSArray $ KM.lookup (K.fromText key) obj
traverseJSPChildSeg (JSPMemberNameSH _) _ = emptyJSArray
traverseJSPChildSeg (JSPSegWildcard JSPWildcard) doc = doc


traverseJSPSelectors :: [JSPSelector] -> JSON.Value -> JSON.Value
traverseJSPSelectors sels doc = JSON.Array $ V.concat $ map traverse' sels
  where
    traverse' = flip traverseJSPSelector doc

traverseJSPSelector :: JSPSelector -> JSON.Value -> V.Vector JSON.Value
traverseJSPSelector (JSPNameSel key) (JSON.Object obj) = maybe V.empty V.singleton $ KM.lookup (K.fromText key) obj
traverseJSPSelector (JSPNameSel _) _ = V.empty
traverseJSPSelector (JSPIndexSel idx) (JSON.Array arr) = maybe V.empty V.singleton $ (if idx >= 0 then (V.!?) arr idx else (V.!?) arr (idx + V.length arr))
traverseJSPSelector (JSPIndexSel _) _ = V.empty
traverseJSPSelector (JSPSliceSel sliceVals) (JSON.Array arr) = traverseJSPSliceSelector sliceVals arr
traverseJSPSelector (JSPSliceSel _) _ = V.empty
traverseJSPSelector (JSPWildSel JSPWildcard) doc = V.singleton doc


traverseJSPSliceSelector :: (Maybe Int, Maybe Int, Int) -> JSON.Array -> V.Vector JSON.Value
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
          V.ifilter (\i _ -> i `mod` (-1 * n) == 0) $ V.reverse $ V.drop (V.length slice `mod` (-1 * n)) slice
        else
          V.ifilter (\i _ -> i `mod` n == 0) slice

emptyJSArray :: JSON.Value
emptyJSArray = JSON.Array V.empty
