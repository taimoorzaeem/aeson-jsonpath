{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query
  ( qQuery
  , qQuerySegment
  , qSegment
  , qSelector
  )
  where

import Control.Monad                   (join)
import Data.Aeson                      (Value)
import Data.Vector                     (Vector)
import Data.Maybe                      (fromMaybe)

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.Aeson.Key        as K
import qualified Data.Vector           as V

import Data.Aeson.JSONPath.Query.Types (Query (..)
                                       , QueryType (..)
                                       , QuerySegment (..)
                                       , Selector (..)
                                       , Segment (..)
                                       , SegmentType (..)
                                       , LogicalOrExpr (..)
                                       , LogicalAndExpr (..)
                                       , BasicExpr (..)
                                       , TestExpr
                                       , ComparisonExpr (..)
                                       , ComparisonOp (..)
                                       , Comparable(..)
                                       , SingularQuery (..)
                                       , SingularQueryType (..)
                                       , SingularQuerySegment (..)
                                       )

import Prelude

qQuery :: Query -> Value -> Value -> Vector Value
qQuery Query{..} root current = case queryType of
  Root    -> foldl applySegment (V.singleton root)    querySegments
  Current -> foldl applySegment (V.singleton current) querySegments
  where
    applySegment :: Vector Value -> QuerySegment -> Vector Value
    applySegment vec seg = join $ V.map (qQuerySegment seg root) vec

qQuerySegment :: QuerySegment -> Value -> Value -> Vector Value
qQuerySegment QuerySegment{..} root current = case segmentType of
  Child      -> joinAfterMap $ V.singleton current
  Descendant -> joinAfterMap $ allElemsRecursive current
  where
    joinAfterMap x = join $ V.map (qSegment segment root) x

qSegment :: Segment -> Value -> Value -> Vector Value
qSegment (Bracketed sels) root current = V.concat $ map (\sel -> qSelector sel root current) sels
qSegment (Dotted key) root current = qSelector (Name key) root current
qSegment WildcardSegment root current = qSelector WildcardSelector root current

-- TODO: Fix this
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

qSelector :: Selector -> Value -> Value -> Vector Value
qSelector (Name key) _ (JSON.Object obj) = maybe V.empty  V.singleton $ KM.lookup (K.fromText key) obj
qSelector (Name _) _ _ = V.empty
qSelector (Index idx) _ (JSON.Array arr) = maybe V.empty V.singleton $ if idx >= 0 then (V.!?) arr idx else (V.!?) arr (idx + V.length arr)
qSelector (Index _) _ _ = V.empty
qSelector (ArraySlice (start,end,step)) _ (JSON.Array arr) = sliceArray start end step arr
qSelector (ArraySlice _) _ _ = V.empty
qSelector (Filter orExpr) root current = filterOrExpr orExpr root current
qSelector WildcardSelector _ cur = case cur of
    (JSON.Object obj) -> V.fromList $ KM.elems obj
    (JSON.Array arr)  -> arr
    _                 -> V.empty


sliceArray :: Maybe Int -> Maybe Int -> Int -> Vector Value -> Vector Value
sliceArray start end step doc =
  case compare step 0 of
    GT -> getSliceForward (maybe 0 normalize start) (maybe len normalize end) step doc
    LT -> getSliceReverse (maybe (len-1) normalize start) (maybe (-1) normalize end) step doc
    EQ -> V.empty
    where
      -- TODO: Refactor this code to make it more pretty
      len = V.length doc
      normalize i = if i >= 0 then i else len + i

      getSliceForward st en stp arr = loop lower V.empty
        where
          (lower,upper) = (min (max st 0) len, min (max en 0) len)

          loop i acc =
            if i < upper
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc

      getSliceReverse st en stp arr = loop upper V.empty
        where
          (lower,upper) = (min (max en (-1)) (len-1), min (max st (-1)) (len-1))

          loop i acc =
            if lower < i
              then loop (i+stp) $ V.snoc acc $ (V.!) arr (normalize i)
            else
              acc


filterOrExpr :: LogicalOrExpr -> Value -> Value -> Vector Value
filterOrExpr expr root (JSON.Object obj) = V.filter (evaluateLogicalOrExpr expr root) (V.fromList $ KM.elems obj)
filterOrExpr expr root (JSON.Array arr) = V.filter (evaluateLogicalOrExpr expr root) arr
filterOrExpr _ _ _ = V.empty

evaluateLogicalOrExpr :: LogicalOrExpr -> Value -> Value -> Bool
evaluateLogicalOrExpr (LogicalOr exprs) root cur = any (\x -> evaluateLogicalAndExpr x root cur) exprs

evaluateLogicalAndExpr :: LogicalAndExpr -> Value -> Value -> Bool
evaluateLogicalAndExpr (LogicalAnd exprs) root cur = all (\x -> evaluateBasicExpr x root cur) exprs

evaluateBasicExpr :: BasicExpr -> Value -> Value -> Bool
evaluateBasicExpr (Paren expr) root cur = evaluateLogicalOrExpr expr root cur
evaluateBasicExpr (NotParen expr) root cur = not $ evaluateLogicalOrExpr expr root cur
evaluateBasicExpr (Test expr) root cur = evaluateTestExpr expr root cur
evaluateBasicExpr (NotTest expr) root cur = not $ evaluateTestExpr expr root cur
evaluateBasicExpr (Comparison expr) root cur = evaluateCompExpr expr root cur


evaluateTestExpr :: TestExpr -> Value -> Value -> Bool
evaluateTestExpr expr root cur = not $ null $ qQuery expr root cur

evaluateCompExpr :: ComparisonExpr -> Value -> Value -> Bool
evaluateCompExpr (Comp leftC op rightC) root cur  = compareVals op (getComparableVal leftC root cur) (getComparableVal rightC root cur)

compareVals :: (Ord a) => ComparisonOp -> a -> a -> Bool
compareVals Less           = (<)
compareVals LessOrEqual    = (<=)
compareVals Greater        = (>)
compareVals GreaterOrEqual = (>=)
compareVals Equal          = (==)
compareVals NotEqual       = (/=)

getComparableVal :: Comparable -> Value -> Value -> Value
getComparableVal (CompLitNum num) _ _ = JSON.Number num
getComparableVal (CompLitString txt) _ _ = JSON.String txt
getComparableVal (CompLitBool bool) _ _ = JSON.Bool bool
getComparableVal CompLitNull _ _ = JSON.Null
getComparableVal (CompSQ SingularQuery{..}) root cur = case singularQueryType of
  RootSQ -> traverseSingularQSegs root singularQuerySegments
  CurrentSQ -> traverseSingularQSegs cur singularQuerySegments

traverseSingularQSegs :: Value -> [SingularQuerySegment] -> Value
traverseSingularQSegs = foldl lookupSingleQSeg

-- TODO: There is a bug here, not existing shouldn't give null
-- See: https://www.rfc-editor.org/rfc/rfc9535#name-examples-6
lookupSingleQSeg :: Value -> SingularQuerySegment -> Value
lookupSingleQSeg (JSON.Object obj) (NameSQSeg txt) = fromMaybe JSON.Null $ KM.lookup (K.fromText txt) obj
lookupSingleQSeg (JSON.Array arr) (IndexSQSeg idx) = fromMaybe JSON.Null $ (V.!?) arr idx
lookupSingleQSeg _ _ = JSON.Null
