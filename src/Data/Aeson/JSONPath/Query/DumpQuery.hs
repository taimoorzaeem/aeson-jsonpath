{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.DumpQuery
  ( dumpQuery )
  where

import Data.Text                       (Text)

import qualified Data.Text             as T

import Data.Aeson.JSONPath.Types

import Prelude

-- | Dump JSONPath Query
dumpQuery :: Query -> Text
dumpQuery Query{..} = case queryType of
  Root    -> "$" <> T.concat (map dumpQuerySegment querySegments)
  Current -> "@" <> T.concat (map dumpQuerySegment querySegments)

-- |
dumpQuerySegment :: QuerySegment Query -> Text
dumpQuerySegment QuerySegment{..} = case segmentType of
  Child      -> dumpSegment True segment
  Descendant -> ".." <> dumpSegment False segment

-- |
dumpSegment :: Bool -> Segment Query -> Text
dumpSegment isChild = \case
  Bracketed selectors -> "[" <> T.intercalate "," (map dumpSelector selectors) <> "]"
  Dotted txt -> "." <> txt
  WildcardSegment -> if isChild then ".*" else "*"

-- |
dumpSelector :: Selector Query -> Text
dumpSelector = \case
  Name txt -> "'" <> txt <> "'"
  Index i -> T.pack (show i)
  ArraySlice (start, end, step) ->
    case (start, end) of
      (Just start', Just end') -> T.pack (show start' <> ":" <> show end' <> ":" <> show step)
      (Just start', Nothing)   -> T.pack (show start' <> "::" <> show step)
      (Nothing, Just end')     -> T.pack (":" <> show end' <> ":" <> show step)
      (Nothing, Nothing)       -> T.pack ("::"<> show step)
  Filter logicalOrExpr -> "?" <>  dumpLogicalOrExpr logicalOrExpr
  WildcardSelector -> "*"

-- |
dumpLogicalOrExpr :: LogicalOrExpr Query -> Text
dumpLogicalOrExpr (LogicalOr andExprs) = T.intercalate " || " (map dumpLogicalAndExpr andExprs)

-- |
dumpLogicalAndExpr :: LogicalAndExpr Query -> Text
dumpLogicalAndExpr (LogicalAnd basicExprs) = T.intercalate " || " (map dumpBasicExpr basicExprs)

-- |
dumpBasicExpr :: BasicExpr Query -> Text
dumpBasicExpr = \case
  Paren orExpr -> "(" <> dumpLogicalOrExpr orExpr <> ")"
  NotParen orExpr -> "!(" <> dumpLogicalOrExpr orExpr <> ")"
  Test testExpr -> dumpTestExpr testExpr
  NotTest testExpr -> "!" <> dumpTestExpr testExpr
  Comparison expr -> dumpComparisonExpr expr

-- |
dumpTestExpr :: TestExpr Query -> Text
dumpTestExpr = \case
  FilterQuery q -> dumpQuery q
  TestFunc funcExpr -> dumpFunctionExpr funcExpr

-- |
dumpComparisonExpr :: ComparisonExpr -> Text
dumpComparisonExpr (Comp compLeft op compRight) = dumpComparable compLeft <> " " <> dumpComparisonOp op <> " " <> dumpComparable compRight

-- |
dumpComparisonOp :: ComparisonOp -> Text
dumpComparisonOp = \case
  Less -> "<"
  LessOrEqual -> "<="
  Greater -> ">"
  GreaterOrEqual -> ">="
  Equal -> "=="
  NotEqual -> "!="

-- |
dumpComparable :: Comparable -> Text
dumpComparable = \case
  CompLit lit -> dumpLiteral lit
  CompSQ sq -> dumpSingularQ sq

-- |
dumpLiteral :: Literal -> Text
dumpLiteral = \case
  LitString txt -> "'" <> txt <> "'"
  LitNum s -> T.pack $ show s
  LitBool b -> T.toLower $ T.pack $ show b
  LitNull -> "null"

-- |
dumpSingularQ :: SingularQuery -> Text
dumpSingularQ SingularQuery{..} = case singularQueryType of
  RootSQ -> "$" <> T.concat (map dumpSingularQSegment singularQuerySegments)
  CurrentSQ -> "@" <> T.concat (map dumpSingularQSegment singularQuerySegments)

-- |
dumpSingularQSegment :: SingularQuerySegment -> Text
dumpSingularQSegment = \case
  NameSQSeg txt -> "." <> txt
  IndexSQSeg i -> "[" <> T.pack (show i) <> "]"

-- |
dumpFunctionExpr :: FunctionExpr Query -> Text
dumpFunctionExpr FunctionSearch{..} =
  "search(" <> dumpFunctionArg functionArg1 <> ", " <> dumpFunctionArg functionArg2 <> ")"

-- |
dumpFunctionArg :: FunctionArg Query -> Text
dumpFunctionArg = \case
  ArgLit lit -> dumpLiteral lit
  ArgQuery q -> dumpQuery q
  ArgLogicExpr e -> dumpLogicalOrExpr e
  ArgFuncExpr f -> dumpFunctionExpr f
