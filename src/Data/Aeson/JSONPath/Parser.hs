{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser
  ( pQuery )
  where

import qualified Data.Scientific                as Sci
import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Aeson.JSONPath.Query.Types (Query (..)
                                       , QueryType (..)
                                       , QuerySegment (..)
                                       , Segment (..)
                                       , SegmentType (..)
                                       , Selector (..)
                                       , LogicalOrExpr (..)
                                       , LogicalAndExpr (..)
                                       , BasicExpr (..)
                                       , ComparisonExpr (..)
                                       , ComparisonOp (..)
                                       , Comparable(..)
                                       , SingularQuery (..)
                                       , SingularQueryType (..)
                                       , SingularQuerySegment (..))
import Data.Functor                  (($>))
import Data.Char                     (ord)
import Data.Maybe                    (isNothing, fromMaybe)
import Data.Scientific               (Scientific, scientific)
import Text.ParserCombinators.Parsec ((<|>))

import Prelude

pQuery :: P.Parser Query
pQuery = pSpaces 
       *> (P.try pRootQuery <|> P.try pCurrentQuery) 
       <* pSpaces 
       <* P.eof

pRootQuery :: P.Parser Query
pRootQuery = do
  P.char '$'
  segs <- P.many pQuerySegment
  return $ Query { queryType = Root, querySegments = segs }

pCurrentQuery :: P.Parser Query
pCurrentQuery = do
  P.char '@'
  segs <- P.many pQuerySegment
  return $ Query { queryType = Current, querySegments = segs }


pQuerySegment :: P.Parser QuerySegment
pQuerySegment = do
  dotdot <- P.optionMaybe (P.try $ P.string "..")
  seg <- pSegment $ isNothing dotdot
  let segType = if isNothing dotdot then Child else Descendant
  return $ QuerySegment { segmentType = segType, segment = seg }

pSegment :: Bool -> P.Parser Segment
pSegment isChild = P.try pBracketed
                <|> P.try (pDotted isChild)
                <|> P.try (pWildcardSeg isChild)


pBracketed :: P.Parser Segment
pBracketed = do
  P.char '['
  sel <- pSelector
  optionalSels <- P.many pCommaSepSelectors
  P.char ']'
  return $ Bracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser Selector
      pCommaSepSelectors = P.try $ P.char ',' *> pSpaces *> pSelector


pDotted :: Bool -> P.Parser Segment
pDotted isChild = do
  (if isChild then P.string "." else P.string "")
  P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
  key <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
  return $ Dotted key


pWildcardSeg :: Bool -> P.Parser Segment
pWildcardSeg isChild = (if isChild then P.string "." else P.string "") *> P.char '*' $> WildcardSegment
  
pSelector :: P.Parser Selector
pSelector = P.try pName
         <|> P.try pIndex
         <|> P.try pSlice 
         <|> P.try pWildcardSel
         <|> P.try pFilter

pName :: P.Parser Selector
pName = Name . T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)
  where
    pSingleQuotted = P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\''
    pDoubleQuotted = P.char '\"' *> P.many (P.noneOf "\"") <* P.char '\"' 

pIndex :: P.Parser Selector
pIndex = do
  idx <- pSignedInt
  P.notFollowedBy $ P.char ':'
  return $ Index idx

pSlice :: P.Parser Selector
pSlice = do
  start <- P.optionMaybe pSignedInt
  P.char ':'
  end <- P.optionMaybe pSignedInt
  step <- P.optionMaybe (P.char ':' *> P.optionMaybe pSignedInt)
  return $ ArraySlice (start, end, case step of
    Just (Just n) -> n
    _ -> 1)


pWildcardSel :: P.Parser Selector
pWildcardSel = P.char '*' $> WildcardSelector


pFilter :: P.Parser Selector
pFilter = do
  P.char '?'
  pSpaces
  Filter <$> pLogicalOrExpr


pLogicalOrExpr :: P.Parser LogicalOrExpr
pLogicalOrExpr = do
  expr <- pLogicalAndExpr
  optionalExprs <- P.many pOrSepLogicalAndExprs
  return $ LogicalOr (expr:optionalExprs)
    where
      pOrSepLogicalAndExprs :: P.Parser LogicalAndExpr
      pOrSepLogicalAndExprs = P.try $ pSpaces *> P.string "||" *> pSpaces *> pLogicalAndExpr

pLogicalAndExpr :: P.Parser LogicalAndExpr
pLogicalAndExpr = do
  expr <- pBasicExpr
  optionalExprs <- P.many pAndSepBasicExprs
  return $ LogicalAnd (expr:optionalExprs)
    where
      pAndSepBasicExprs :: P.Parser BasicExpr
      pAndSepBasicExprs = P.try $ pSpaces *> P.string "&&" *> pSpaces *> pBasicExpr

pBasicExpr :: P.Parser BasicExpr
pBasicExpr = P.try pParenExpr
          <|> P.try pComparisonExpr
          <|> P.try pTestExpr

pParenExpr :: P.Parser BasicExpr
pParenExpr = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  P.char '('
  pSpaces
  expr <- pLogicalOrExpr
  pSpaces
  P.char ')'
  let parenExp = if isNothing notOp then Paren expr else NotParen expr
  return parenExp

pTestExpr :: P.Parser BasicExpr
pTestExpr = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  q <- P.try pRootQuery <|> P.try pCurrentQuery
  let testExp = if isNothing notOp then Test q else NotTest q
  return testExp

pComparisonExpr :: P.Parser BasicExpr
pComparisonExpr = do
  leftC <- pComparable
  pSpaces
  compOp <- pComparisonOp
  pSpaces
  Comparison . Comp leftC compOp <$> pComparable

pComparisonOp :: P.Parser ComparisonOp
pComparisonOp = P.try (P.string ">=" $> GreaterOrEqual)
                <|> P.try (P.string "<=" $> LessOrEqual)
                <|> P.try (P.char '>' $> Greater)
                <|> P.try (P.char '<' $> Less)
                <|> P.try (P.string "!=" $> NotEqual)
                <|> P.try (P.string "==" $> Equal)

pComparable :: P.Parser Comparable
pComparable = P.try pCompLitString
              <|> P.try pCompLitNum
              <|> P.try pCompLitBool
              <|> P.try pCompLitNull
              <|> P.try pCompSQ

pCompLitString :: P.Parser Comparable
pCompLitString = CompLitString . T.pack <$> (P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\'')

pCompLitNum :: P.Parser Comparable
pCompLitNum = CompLitNum <$> (P.try pDoubleScientific <|> P.try pScientific)

pCompLitBool :: P.Parser Comparable
pCompLitBool = CompLitBool <$> (P.try (P.string "true" $> True) <|> P.try (P.string "false" $> False))

pCompLitNull :: P.Parser Comparable
pCompLitNull = P.string "null" $> CompLitNull

pCompSQ :: P.Parser Comparable
pCompSQ = CompSQ <$> (P.try pCurrentSingleQ <|> P.try pRootSingleQ)

pCurrentSingleQ :: P.Parser SingularQuery
pCurrentSingleQ = do
  P.char '@'
  segs <- P.many pSingularQuerySegment
  return $ SingularQuery { singularQueryType = CurrentSQ, singularQuerySegments = segs }

pRootSingleQ :: P.Parser SingularQuery
pRootSingleQ = do
  P.char '$'
  segs <- P.many pSingularQuerySegment
  return $ SingularQuery { singularQueryType = RootSQ, singularQuerySegments = segs }

pSingularQuerySegment :: P.Parser SingularQuerySegment
pSingularQuerySegment = P.try pSingularQNameSeg <|> P.try pSingularQIndexSeg

pSingularQNameSeg :: P.Parser SingularQuerySegment
pSingularQNameSeg = P.try pSingularQNameBracketed <|> P.try pSingularQNameDotted
  where
    pSingularQNameBracketed = do
      let pSingleQuotted = P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\''
      let pDoubleQuotted = P.char '\"' *> P.many (P.noneOf "\"") <* P.char '\"'
      P.char '['
      name <- T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)
      P.char ']'
      return $ NameSQSeg name

    pSingularQNameDotted = do
      P.char '.'
      P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
      name <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
      return $ NameSQSeg name

pSingularQIndexSeg :: P.Parser SingularQuerySegment
pSingularQIndexSeg = do
  P.char '['
  idx <- pSignedInt
  P.char ']'
  return $ IndexSQSeg idx

pSignedInt :: P.Parser Int
pSignedInt = do
  sign <- P.optionMaybe $ P.char '-'
  num <- read <$> P.many1 P.digit
  return $ 
    case sign of
      Just _ -> -num
      Nothing -> num

pSignedInteger :: P.Parser Integer
pSignedInteger = do
  sign <- P.optionMaybe $ P.char '-'
  num <- read <$> P.many1 P.digit
  return $
    case sign of
      Just _ -> -num
      Nothing -> num

pScientific :: P.Parser Scientific
pScientific = do
  integer <- pSignedInteger
  int <- P.optionMaybe (P.char 'e' *> pSignedInt)
  return $ scientific integer (fromMaybe 0 int)

pDoubleScientific :: P.Parser Scientific
pDoubleScientific = do
  whole <- P.many1 P.digit
  P.char '.'
  frac <- P.many1 P.digit
  let num = read (whole ++ "." ++ frac) :: Double
  return $ Sci.fromFloatDigits num

pUnicodeChar :: P.Parser Char
pUnicodeChar = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x80 && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)

-- https://www.rfc-editor.org/rfc/rfc9535#name-syntax
pSpaces :: P.Parser [Char]
pSpaces = P.many (P.oneOf " \n\r\t")
