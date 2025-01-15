{-# LANGUAGE RecordWildCards #-}
{- |
Module      : Data.Aeson.JSONPath.Query
Description : Algorithm for query runner
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable

This module contains core functions that runs the query on 'Value'.
-}
module Data.Aeson.JSONPath.Query
  ( Queryable (..) )
  where

import Control.Monad                   (join)
import Data.Aeson                      (Value)
import Data.Vector                     (Vector)

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.Aeson.Key        as K
import qualified Data.Text             as T
import qualified Data.Vector           as V

import Data.Aeson.JSONPath.Query.Types

import Prelude

-- |
class Queryable a where
  query'        :: a -> Value -> Value -> Vector Value
  queryLocated' :: a -> Value -> Value -> String -> Vector (String,Value)

instance Queryable Query where
  query'        = qQuery
  queryLocated' = qQueryLocated

instance Queryable QuerySegment where
  query'        = qQuerySegment
  queryLocated' = qQuerySegmentLocated

instance Queryable Segment where
  query'        = qSegment
  queryLocated' = qSegmentLocated

instance Queryable Selector where
  query'        = qSelector
  queryLocated' = qSelectorLocated

-- TODO: the whole module is kinda bloated, refactor
--    Blockers with refactoring: qQuery is used recursively

qQuery :: Query -> Value -> Value -> Vector Value
qQuery Query{..} root current = case queryType of
  Root    -> foldl applySegment (V.singleton root)    querySegments
  Current -> foldl applySegment (V.singleton current) querySegments
  where
    applySegment :: Vector Value -> QuerySegment -> Vector Value
    applySegment vec seg = join $ V.map (qQuerySegment seg root) vec

qQueryLocated :: Query -> Value -> Value -> String -> Vector (String,Value)
qQueryLocated Query{..} root current loc = case queryType of
  Root    -> foldl applySegment (V.singleton (loc,root))    querySegments
  Current -> foldl applySegment (V.singleton (loc,current)) querySegments
  where
    applySegment :: Vector (String,Value) -> QuerySegment -> Vector (String,Value)
    applySegment vec seg = join $ V.map (\(location,cur) -> qQuerySegmentLocated seg root cur location) vec

qQuerySegment :: QuerySegment -> Value -> Value -> Vector Value
qQuerySegment QuerySegment{..} root current = case segmentType of
  Child      -> joinAfterMap $ V.singleton current
  Descendant -> joinAfterMap $ allElemsRecursive current
  where
    joinAfterMap x = join $ V.map (qSegment segment root) x

qQuerySegmentLocated :: QuerySegment -> Value -> Value -> String -> Vector (String, Value)
qQuerySegmentLocated QuerySegment{..} root current loc = case segmentType of
  Child      -> joinAfterMap $ V.singleton (loc,current)
  Descendant -> joinAfterMap $ allElemsRecursiveLocated (loc,current)
  where
    joinAfterMap x = join $ V.map (\(location,cur) -> qSegmentLocated segment root cur location) x

qSegment :: Segment -> Value -> Value -> Vector Value
qSegment (Bracketed sels) root current = V.concat $ map (\sel -> qSelector sel root current) sels
qSegment (Dotted key) root current = qSelector (Name key) root current
qSegment WildcardSegment root current = qSelector WildcardSelector root current

qSegmentLocated :: Segment -> Value -> Value -> String -> Vector (String,Value)
qSegmentLocated (Bracketed sels) root current loc = V.concat $ map (\sel -> qSelectorLocated sel root current loc) sels
qSegmentLocated (Dotted key) root current loc = qSelectorLocated (Name key) root current loc
qSegmentLocated WildcardSegment root current loc = qSelectorLocated WildcardSelector root current loc


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


qSelector :: Selector -> Value -> Value -> Vector Value
qSelector (Name key) _ (JSON.Object obj) = maybe V.empty  V.singleton $ KM.lookup (K.fromText key) obj
qSelector (Name _) _ _ = V.empty
qSelector (Index idx) _ (JSON.Array arr) = maybe V.empty V.singleton $ if idx >= 0 then (V.!?) arr idx else (V.!?) arr (idx + V.length arr)
qSelector (Index _) _ _ = V.empty
qSelector (ArraySlice startEndStep) _ (JSON.Array arr) = sliceArray startEndStep arr
qSelector (ArraySlice _) _ _ = V.empty
qSelector (Filter orExpr) root current = filterOrExpr orExpr root current
qSelector WildcardSelector _ cur = case cur of
    (JSON.Object obj) -> V.fromList $ KM.elems obj
    (JSON.Array arr)  -> arr
    _                 -> V.empty

qSelectorLocated :: Selector -> Value -> Value -> String -> Vector (String,Value)
qSelectorLocated (Name key) _ (JSON.Object obj) loc = maybe V.empty (\x-> V.singleton (loc ++ "['" ++ T.unpack key ++ "']", x)) $ KM.lookup (K.fromText key) obj
qSelectorLocated (Name _) _ _ _ = V.empty
qSelectorLocated (Index idx) _ (JSON.Array arr) loc = maybe V.empty (\x-> V.singleton (newLocation, x)) $ (V.!?) arr (getIndex idx)
  where
    newLocation = loc ++ "[" ++ show (getIndex idx) ++ "]"
    getIndex i = if i >= 0 then i else i + V.length arr
qSelectorLocated (Index _) _ _ _ = V.empty
qSelectorLocated (ArraySlice (start,end,step)) _ (JSON.Array arr) loc = sliceArrayLocated (start,end,step) $ V.zip (V.fromList locs) arr
  where
    locs = [ loc ++ "[" ++ show i ++ "]" | i <- indices ]
    indices = [0..(V.length arr - 1)]
qSelectorLocated (ArraySlice _) _ _ _ = V.empty
qSelectorLocated (Filter orExpr) root cur loc = filterOrExprLocated orExpr root cur loc
qSelectorLocated WildcardSelector _ cur loc = case cur of
    (JSON.Object obj) -> V.fromList $ zip (locsWithKeys obj) (KM.elems obj)
    (JSON.Array arr)  -> V.zip (V.fromList (locsWithIdxs arr)) arr
    _                 -> V.empty
    where
      locsWithKeys obj = map (\x -> loc ++ "['" ++ K.toString x ++ "']") (KM.keys obj)
      locsWithIdxs arr = map (\x -> loc ++ "[" ++ show x ++ "]") [0..(V.length arr)]


sliceArray :: (Maybe Int, Maybe Int, Int) -> Vector Value -> Vector Value
sliceArray (start,end,step) vec =
  case compare step 0 of
    GT -> getSliceForward (maybe 0 normalize start) (maybe len normalize end) step vec
    LT -> getSliceReverse (maybe (len-1) normalize start) (maybe (-1) normalize end) step vec
    EQ -> V.empty
    where
      -- TODO: Looks kinda ugly, make it pretty <3
      len = V.length vec
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


sliceArrayLocated :: (Maybe Int, Maybe Int, Int) -> Vector (String,Value) -> Vector (String,Value)
sliceArrayLocated (start,end,step) vec =
  case compare step 0 of
    GT -> getSliceForward (maybe 0 normalize start) (maybe len normalize end) step vec
    LT -> getSliceReverse (maybe (len-1) normalize start) (maybe (-1) normalize end) step vec
    EQ -> V.empty
    where
      -- TODO: Looks kinda ugly, make it pretty <3
      len = V.length vec
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

filterOrExprLocated :: LogicalOrExpr -> Value -> Value -> String -> Vector (String,Value)
filterOrExprLocated expr root (JSON.Object obj) loc = V.filter (\(_,x) -> evaluateLogicalOrExpr expr root x) (V.fromList $ zip locsWithKeys (KM.elems obj))
  where
    locsWithKeys = map (\x -> loc ++ "['" ++ K.toString x ++ "']") (KM.keys obj)
filterOrExprLocated expr root (JSON.Array arr) loc = V.filter (\(_,x) -> evaluateLogicalOrExpr expr root x) (V.zip (V.fromList locsWithIdxs) arr)
  where
    locsWithIdxs = map (\x -> loc ++ "[" ++ show x ++ "]") [0..(V.length arr - 1)]
filterOrExprLocated _ _ _ _ = V.empty


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


compareVals :: ComparisonOp -> Maybe Value -> Maybe Value -> Bool
compareVals Less (Just (JSON.String s1)) (Just (JSON.String s2)) = s1 < s2
compareVals Less (Just (JSON.Number n1)) (Just (JSON.Number n2)) = n1 < n2
compareVals Less _  _ = False

compareVals LessOrEqual    o1 o2 = compareVals Less o1 o2 || compareVals Equal o1 o2
compareVals Greater        o1 o2 = compareVals Less o2 o1
compareVals GreaterOrEqual o1 o2 = compareVals Less o2 o1 || compareVals Equal o1 o2
compareVals Equal          o1 o2 = o1 == o2
compareVals NotEqual       o1 o2 = o1 /= o2


getComparableVal :: Comparable -> Value -> Value -> Maybe Value
getComparableVal (CompLitNum num) _ _ = Just $ JSON.Number num
getComparableVal (CompLitString txt) _ _ = Just $ JSON.String txt
getComparableVal (CompLitBool bool) _ _ = Just $ JSON.Bool bool
getComparableVal CompLitNull _ _ = Just JSON.Null
getComparableVal (CompSQ SingularQuery{..}) root cur = case singularQueryType of
  RootSQ -> traverseSingularQSegs (Just root) singularQuerySegments
  CurrentSQ -> traverseSingularQSegs (Just cur) singularQuerySegments


traverseSingularQSegs :: Maybe Value -> [SingularQuerySegment] -> Maybe Value
traverseSingularQSegs = foldl lookupSingleQSeg


-- TODO: There is a bug here, not existing shouldn't give null
-- See: https://www.rfc-editor.org/rfc/rfc9535#name-examples-6
lookupSingleQSeg :: Maybe Value -> SingularQuerySegment -> Maybe Value
lookupSingleQSeg (Just (JSON.Object obj)) (NameSQSeg txt) = KM.lookup (K.fromText txt) obj
lookupSingleQSeg (Just (JSON.Array arr)) (IndexSQSeg idx) = (V.!?) arr idx
lookupSingleQSeg _ _ = Nothing
