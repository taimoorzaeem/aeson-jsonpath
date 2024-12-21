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
traverseJSPSelector (JSPIndexSel idx) (JSON.Array arr) = maybe V.empty V.singleton $ (V.!?) arr idx
traverseJSPSelector (JSPIndexSel _) _ = V.empty
traverseJSPSelector (JSPSliceSel sliceVals) (JSON.Array arr) = traverseJSPSliceSelector sliceVals arr
traverseJSPSelector (JSPSliceSel _) _ = V.empty
traverseJSPSelector (JSPWildSel JSPWildcard) doc = V.singleton doc


traverseJSPSliceSelector :: (Maybe Int, Maybe Int, Int) -> JSON.Array -> V.Vector JSON.Value
traverseJSPSliceSelector (Just start, Just end, 1) doc = V.slice (normalize start) ((normalize end)-1) doc
  where
    len = V.length doc
    normalize i = if i >= 0 then i else len + i
traverseJSPSliceSelector _ _ = V.empty

emptyJSArray :: JSON.Value
emptyJSArray = JSON.Array V.empty
