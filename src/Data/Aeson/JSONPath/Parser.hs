{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{- |
Module      : Data.Aeson.JSONPath.Parser
Description : 
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable

Parses raw query to Haskell Algebraic Data Types
-}
module Data.Aeson.JSONPath.Parser
  ( pQuery )
  where

import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Functor                  (($>))
import Data.Char                     (ord, chr)
import Data.Maybe                    (isNothing, fromMaybe)
import Data.Scientific               (Scientific, scientific)
import GHC.Num                       (integerFromInt, integerToInt)
import Text.ParserCombinators.Parsec ((<|>))

import Data.Aeson.JSONPath.Types

import Prelude

-- | Query parser
pQuery :: P.Parser Query
pQuery = P.try pRootQuery <* P.eof

pRootQuery :: P.Parser Query
pRootQuery = do
  P.char '$'
  segs <- P.many (pSpaces *> pQuerySegment (P.try pRootQuery <|> P.try pCurrentQuery))
  return $ Query { queryType = Root, querySegments = segs }

pCurrentQuery :: P.Parser Query
pCurrentQuery = do
  P.char '@'
  segs <- P.many $ pQuerySegment (P.try pRootQuery <|> P.try pCurrentQuery)
  return $ Query { queryType = Current, querySegments = segs }


pQuerySegment :: P.Parser a -> P.Parser (QuerySegment a)
pQuerySegment pQ = do
  dotdot <- P.optionMaybe (P.try $ P.string "..")
  seg <- pSegment pQ $ isNothing dotdot
  let segType = if isNothing dotdot then Child else Descendant
  return $ QuerySegment { segmentType = segType, segment = seg }

pSegment :: P.Parser a -> Bool -> P.Parser (Segment a)
pSegment pQ isChild
        = P.try (pBracketed pQ)
       <|> P.try (pDotted isChild)
       <|> P.try (pWildcardSeg isChild)

pBracketed :: P.Parser a -> P.Parser (Segment a)
pBracketed pQ = do
  P.char '['
  pSpaces
  sel <- pSelector pQ
  optionalSels <- P.many $ pCommaSepSelectors pQ
  pSpaces
  P.char ']'
  return $ Bracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser a -> P.Parser (Selector a)
      pCommaSepSelectors p = P.try $ pSpaces *> P.char ',' *> pSpaces *> pSelector p


pDotted :: Bool -> P.Parser (Segment a)
pDotted isChild = do
  (if isChild then P.string "." else P.string "")
  P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
  key <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
  return $ Dotted key


pWildcardSeg :: Bool -> P.Parser (Segment a)
pWildcardSeg isChild = (if isChild then P.string "." else P.string "") *> P.char '*' $> WildcardSegment
  
pSelector :: P.Parser a -> P.Parser (Selector a)
pSelector pQ = P.try pName
                   <|> P.try pSlice 
                   <|> P.try pIndex
                   <|> P.try pWildcardSel
                   <|> P.try (pFilter pQ)

pName :: P.Parser (Selector a)
pName = Name . T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)

pIndex :: P.Parser (Selector a)
pIndex = Index <$> pSignedInt

pSlice :: P.Parser (Selector a)
pSlice = do
  start <- P.optionMaybe (pSignedInt <* pSpaces)
  P.char ':'
  pSpaces
  end <- P.optionMaybe (pSignedInt <* pSpaces)
  step <- P.optionMaybe (P.char ':' *> P.optionMaybe (pSpaces *> pSignedInt))
  return $ ArraySlice (start, end, case step of
    Just (Just n) -> n
    _ -> 1)


pWildcardSel :: P.Parser (Selector a)
pWildcardSel = P.char '*' $> WildcardSelector


pFilter :: P.Parser a -> P.Parser (Selector a)
pFilter pQ = do
  P.char '?'
  pSpaces
  Filter <$> pLogicalOrExpr pQ


pLogicalOrExpr :: P.Parser a -> P.Parser (LogicalOrExpr a)
pLogicalOrExpr pQ = do
  expr <- pLogicalAndExpr pQ
  optionalExprs <- P.many $ pOrSepLogicalAndExprs pQ
  return $ LogicalOr (expr:optionalExprs)
    where
      pOrSepLogicalAndExprs :: P.Parser a -> P.Parser (LogicalAndExpr a)
      pOrSepLogicalAndExprs pQ' = P.try $ pSpaces *> P.string "||" *> pSpaces *> pLogicalAndExpr pQ'

pLogicalAndExpr :: P.Parser a -> P.Parser (LogicalAndExpr a)
pLogicalAndExpr pQ = do
  expr <- pBasicExpr pQ
  optionalExprs <- P.many $ pAndSepBasicExprs pQ
  return $ LogicalAnd (expr:optionalExprs)
    where
      pAndSepBasicExprs :: P.Parser a -> P.Parser (BasicExpr a)
      pAndSepBasicExprs pQ' = P.try $ pSpaces *> P.string "&&" *> pSpaces *> pBasicExpr pQ'

pBasicExpr :: P.Parser a -> P.Parser (BasicExpr a)
pBasicExpr pQ 
  = P.try (pParenExpr pQ)
 <|> P.try pComparisonExpr
 <|> P.try (pTestExpr pQ)

pParenExpr :: P.Parser a -> P.Parser (BasicExpr a)
pParenExpr pQ = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  P.char '('
  pSpaces
  expr <- pLogicalOrExpr pQ
  pSpaces
  P.char ')'
  let parenExp = if isNothing notOp then Paren expr else NotParen expr
  return parenExp

pTestExpr :: P.Parser a -> P.Parser (BasicExpr a)
pTestExpr pQ = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  q <- pQ
  let testExp = if isNothing notOp then Test q else NotTest q
  return testExp

pComparisonExpr :: P.Parser (BasicExpr a)
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
pCompLitString = CompLitString . T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)

pCompLitNum :: P.Parser Comparable
pCompLitNum = CompLitNum 
           <$> (P.try (P.string "-0" $> (0 :: Scientific)) -- edge case
           <|> P.try pDoubleScientific 
           <|> P.try pScientific)

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
  P.notFollowedBy (P.string "-0" *> P.optional P.digit) -- no leading -011... etc
  P.notFollowedBy (P.char   '0' *> P.digit) -- no leading 011... etc
  sign <- P.optionMaybe $ P.char '-'
  num <- (read <$> P.many1 P.digit) :: P.Parser Integer
  checkNumOutOfRange num sign
  where
    minInt = -9007199254740991
    maxInt = 9007199254740991
    checkNumOutOfRange num (Just _) =
      if -num < minInt then fail "out of range"
      else return $ integerToInt (-num)

    checkNumOutOfRange num Nothing =
      if num > maxInt then fail "out of range"
      else return $ integerToInt num

-- TODO: Fix Double parse error  "1.12e+23"
pScientific :: P.Parser Scientific
pScientific = do
  mantissa <- pSignedInt
  expo <- P.optionMaybe (P.oneOf "eE" *> pExponent)
  return $ scientific (integerFromInt mantissa) (fromMaybe 0 expo)

pDoubleScientific :: P.Parser Scientific
pDoubleScientific = do
  whole <- P.many1 P.digit
  P.char '.'
  frac <- P.many1 P.digit
  expo <- P.optionMaybe (P.oneOf "eE" *> pExponent)
  let num = read (whole ++ "." ++ frac ++ maybe "" (\x -> "e" ++ show x) expo) :: Scientific
  return num

pExponent :: P.Parser Int
pExponent = do
  sign <- P.optionMaybe (P.oneOf "+-")
  num <- read <$> P.many1 P.digit
  return $ case sign of
    Just '-' -> -num
    _        -> num

pUnicodeChar :: P.Parser Char
pUnicodeChar = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x80 && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)

-- https://www.rfc-editor.org/rfc/rfc9535#name-syntax
pSpaces :: P.Parser [Char]
pSpaces = P.many (P.oneOf " \n\r\t")

pSingleQuotted :: P.Parser String
pSingleQuotted = P.char '\'' *> P.many inQuote <* P.char '\''
  where
    inQuote = P.try pUnescaped
           <|> P.try (P.char '\"')
           <|> P.try (P.string "\\\'" $> '\'')
           <|> P.try pEscaped

pDoubleQuotted :: P.Parser String
pDoubleQuotted = P.char '\"' *> P.many inQuote <* P.char '\"'
  where
    inQuote = P.try pUnescaped
           <|> P.try (P.char '\'')
           <|> P.try (P.string "\\\"" $> '\"')
           <|> P.try pEscaped

pUnescaped :: P.Parser Char
pUnescaped = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x20 && code <= 0x21) ||
      (code >= 0x23 && code <= 0x26) ||
      (code >= 0x28 && code <= 0x5B) ||
      (code >= 0x5D && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)

pEscaped :: P.Parser Char
pEscaped = do
  P.char '\\'
  P.try pEscapees <|> P.try pHexUnicode
  where
    pEscapees = P.try (P.char 'b' $> '\b')
            <|> P.try (P.char 'f' $> '\f')
            <|> P.try (P.char 'n' $> '\n')
            <|> P.try (P.char 'r' $> '\r')
            <|> P.try (P.char 't' $> '\t')
            <|> P.try (P.char '/')
            <|> P.try (P.char '\\')

pHexUnicode :: P.Parser Char
pHexUnicode = P.try pNonSurrogate <|> P.try pSurrogatePair
  where
    pNonSurrogate = P.try pNonSurrogateFirst <|> P.try pNonSurrogateSecond
      where
        pNonSurrogateFirst = do
          P.char 'u'
          c1 <- P.digit <|> P.oneOf "AaBbCcEeFf"
          c2 <- P.hexDigit
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return $ chr (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

        pNonSurrogateSecond = do
          P.char 'u'
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "01234567"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return $ chr (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

    pSurrogatePair = do
      P.char 'u'
      high <- pHighSurrogate
      P.char '\\'
      P.char 'u'
      fromHighAndLow high <$> pLowSurrogate
      where
        pHighSurrogate = do
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "89AaBb"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

        pLowSurrogate = do
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "CcDdEeFf"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return (read ("0x" ++ [c1,c2,c3,c4]) :: Int)
          
        fromHighAndLow hi lo = chr $ ((hi - 0xD800) * 0x400) + (lo - 0xDC00) + 0x10000
