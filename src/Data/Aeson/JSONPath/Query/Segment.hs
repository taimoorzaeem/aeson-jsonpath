{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.Segment
  ( qQuerySegment
  , qQuerySegmentLocated
  )
  where

import Control.Monad (join)
import Data.Aeson    (Value)
import Data.Vector   (Vector)

import qualified Data.Aeson         as JSON
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Vector        as V

import Data.Aeson.JSONPath.Types
import Data.Aeson.JSONPath.Query.Selector

import Prelude

qQuerySegment :: QuerySegment Query -> QueryState -> Vector Value
qQuerySegment QuerySegment{..} qS@QueryState{..} = case segmentType of
  Child      -> joinAfterMap $ V.singleton curVal
  Descendant -> joinAfterMap $ allElemsRecursive curVal
  where
    joinAfterMap vec = join $ V.map (\cur -> qSegment segment qS{ curVal = cur }) vec

qQuerySegmentLocated :: QuerySegment Query -> QueryState-> String -> Vector (String, Value)
qQuerySegmentLocated QuerySegment{..} qS@QueryState{..} loc = case segmentType of
  Child      -> joinAfterMap $ V.singleton (loc,curVal)
  Descendant -> joinAfterMap $ allElemsRecursiveLocated (loc,curVal)
  where
    joinAfterMap vec = join $ V.map (\(location,cur) -> qSegmentLocated segment qS{ curVal = cur } location) vec

qSegment :: Segment Query -> QueryState -> Vector Value
qSegment (Bracketed sels) qS = V.concat $ map (`qSelector` qS) sels
qSegment (Dotted key) qS = qSelector (Name key) qS
qSegment WildcardSegment qS = qSelector WildcardSelector qS

qSegmentLocated :: Segment Query -> QueryState -> String -> Vector (String,Value)
qSegmentLocated (Bracketed sels) qS loc = V.concat $ map (\sel -> qSelectorLocated sel qS loc) sels
qSegmentLocated (Dotted key) qS loc = qSelectorLocated (Name key) qS loc
qSegmentLocated WildcardSegment qS loc = qSelectorLocated WildcardSelector qS loc

-- TODO: Looks kinda ugly, make it pretty <3
allElemsRecursive :: Value -> Vector Value
allElemsRecursive o@(JSON.Object obj) = V.concat [
    V.singleton o,
    V.concat $ map allElemsRecursive (KM.elems obj)
  ]
allElemsRecursive a@(JSON.Array arr) = V.concat [
    V.singleton a,
    V.concat $ map allElemsRecursive (V.toList arr)
  ]
allElemsRecursive _ = V.empty

-- TODO: Looks kinda ugly, make it pretty <3
allElemsRecursiveLocated :: (String,Value) -> Vector (String,Value)
allElemsRecursiveLocated (loc, o@(JSON.Object obj)) = V.concat [
    V.singleton (loc,o),
    V.concat $ zipWith (curry allElemsRecursiveLocated) (replicate (length (KM.elems obj)) loc) (KM.elems obj)
  ]
allElemsRecursiveLocated (loc, a@(JSON.Array arr)) = V.concat [
    V.singleton (loc,a),
    V.concat $ zipWith (curry allElemsRecursiveLocated) (replicate (V.length arr) loc) (V.toList arr)
  ]
allElemsRecursiveLocated _ = V.empty
