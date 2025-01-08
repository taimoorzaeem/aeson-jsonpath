{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser
  ( pJSPQuery )
  where

import qualified Data.Scientific                as Sci
import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Aeson.JSONPath.Types     (JSPQuery (..)
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
                                     , JSPSingleQSegment (..))
import Data.Functor                  (($>))
import Data.Char                     (ord)
import Data.Maybe                    (isJust, fromMaybe)
import Data.Scientific               (Scientific, scientific)
import Text.ParserCombinators.Parsec ((<|>))

import Prelude

pJSPQuery :: P.Parser JSPQuery
pJSPQuery = do
  pSpaces
  P.char '$'
  segs <- P.many pJSPSegment
  pSpaces
  P.eof
  return $ JSPRoot segs

pJSPSelector :: P.Parser JSPSelector
pJSPSelector = P.try pJSPNameSel 
            <|> P.try pJSPIndexSel
            <|> P.try pJSPSliceSel 
            <|> P.try pJSPWildSel
            <|> P.try pJSPFilterSel

pJSPNameSel :: P.Parser JSPSelector
pJSPNameSel = JSPNameSel . T.pack <$> (P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\'')

pJSPIndexSel :: P.Parser JSPSelector
pJSPIndexSel = do
  num <- pSignedInt
  P.notFollowedBy $ P.char ':'
  return $ JSPIndexSel num

pJSPSliceSel :: P.Parser JSPSelector
pJSPSliceSel = do
  start <- P.optionMaybe pSignedInt
  P.char ':'
  end <- P.optionMaybe pSignedInt
  step <- P.optionMaybe (P.char ':' *> P.optionMaybe pSignedInt)
  return $ JSPSliceSel (start, end, case step of
    Just (Just n) -> n
    _ -> 1)


pJSPWildSel :: P.Parser JSPSelector
pJSPWildSel = JSPWildSel <$> (P.char '*' $> JSPWildcard)

pJSPFilterSel :: P.Parser JSPSelector
pJSPFilterSel = do
  P.char '?'
  pSpaces
  JSPFilterSel <$> pJSPLogicalExpr

pJSPSegment :: P.Parser JSPSegment
pJSPSegment = pJSPChildSegment <|> pJSPDescSegment

pJSPChildSegment :: P.Parser JSPSegment
pJSPChildSegment = 
  JSPChildSeg <$> (P.try pJSPChildBracketed 
                  <|> P.try pJSPChildMemberNameSH 
                  <|> P.try pJSPChildWildSeg)

pJSPChildBracketed :: P.Parser JSPChildSegment
pJSPChildBracketed =  do
  P.char '['
  sel <- pJSPSelector
  optionalSels <- P.many pCommaSepSelectors
  P.char ']'
  return $ JSPChildBracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser JSPSelector
      pCommaSepSelectors = P.char ',' *> pSpaces *> pJSPSelector

pJSPChildMemberNameSH :: P.Parser JSPChildSegment
pJSPChildMemberNameSH = do
  P.char '.'
  P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
  val <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
  return (JSPChildMemberNameSH val)

pJSPChildWildSeg :: P.Parser JSPChildSegment
pJSPChildWildSeg = JSPChildWildSeg <$> (P.string ".*" $> JSPWildcard)


pJSPDescSegment :: P.Parser JSPSegment
pJSPDescSegment = 
  JSPDescSeg <$> (P.try pJSPDescBracketed 
                  <|> P.try pJSPDescMemberNameSH 
                  <|> P.try pJSPDescWildSeg)

pJSPDescBracketed :: P.Parser JSPDescSegment
pJSPDescBracketed =  do
  P.string ".."
  P.char '['
  sel <- pJSPSelector
  optionalSels <- P.many pCommaSepSelectors
  P.char ']'
  return $ JSPDescBracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser JSPSelector
      pCommaSepSelectors = P.char ',' *> pSpaces *> pJSPSelector

pJSPDescMemberNameSH :: P.Parser JSPDescSegment
pJSPDescMemberNameSH = do
  P.string ".."
  P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
  val <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
  return (JSPDescMemberNameSH val)

pJSPDescWildSeg :: P.Parser JSPDescSegment
pJSPDescWildSeg = JSPDescWildSeg <$> (P.string "..*" $> JSPWildcard)

pJSPLogicalExpr :: P.Parser JSPLogicalExpr
pJSPLogicalExpr = do
  expr <- pJSPLogicalAndExpr
  optionalExprs <- P.many pOrSepLogicalAndExprs
  return $ JSPLogicalOr (expr:optionalExprs)
    where
      pOrSepLogicalAndExprs :: P.Parser JSPLogicalAndExpr
      pOrSepLogicalAndExprs = P.try $ pSpaces *> P.string "||" *> pSpaces *> pJSPLogicalAndExpr

pJSPLogicalAndExpr :: P.Parser JSPLogicalAndExpr
pJSPLogicalAndExpr = do
  expr <- pJSPBasicExpr
  optionalExprs <- P.many pAndSepBasicExprs
  return $ JSPLogicalAnd (expr:optionalExprs)
    where
      pAndSepBasicExprs :: P.Parser JSPBasicExpr
      pAndSepBasicExprs = P.try $ pSpaces *> P.string "&&" *> pSpaces *> pJSPBasicExpr

pJSPBasicExpr :: P.Parser JSPBasicExpr
pJSPBasicExpr = P.try pJSPParenExpr
             <|> P.try pJSPComparisonExpr
             <|> P.try pJSPTestExpr

pJSPParenExpr :: P.Parser JSPBasicExpr
pJSPParenExpr = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  P.char '('
  pSpaces
  expr <- pJSPLogicalExpr
  pSpaces
  P.char ')'
  let parenExp = if isJust notOp then JSPParenFalse expr else JSPParenTrue expr
  return $ JSPParen parenExp

pJSPTestExpr :: P.Parser JSPBasicExpr
pJSPTestExpr = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  q <- pJSPFilterQuery
  let testExp = if isJust notOp then JSPTestFalse q else JSPTestTrue q
  return $ JSPTest testExp

pJSPFilterQuery :: P.Parser JSPFilterQuery
pJSPFilterQuery = P.try pJSPRelQuery <|> P.try pJSPRootQuery

pJSPRelQuery :: P.Parser JSPFilterQuery
pJSPRelQuery = do
  P.char '@'
  segs <- P.many pJSPSegment
  return $ JSPFilterRelQ segs

pJSPRootQuery :: P.Parser JSPFilterQuery
pJSPRootQuery = do
  P.char '$'
  segs <- P.many pJSPSegment
  return $ JSPFilterRootQ segs

pJSPComparisonExpr :: P.Parser JSPBasicExpr
pJSPComparisonExpr = do
  leftC <- pJSPComparable
  pSpaces
  compOp <- pJSPComparisonOp
  pSpaces
  JSPComparison . JSPComp leftC compOp <$> pJSPComparable

pJSPComparisonOp :: P.Parser JSPComparisonOp
pJSPComparisonOp = P.try (P.string ">=" $> JSPGreaterOrEqual)
                <|> P.try (P.string "<=" $> JSPLessOrEqual)
                <|> P.try (P.char '>' $> JSPGreater)
                <|> P.try (P.char '<' $> JSPLess)
                <|> P.try (P.string "!=" $> JSPNotEqual)
                <|> P.try (P.string "==" $> JSPEqual)

pJSPComparable :: P.Parser JSPComparable
pJSPComparable = P.try pJSPCompLitString
              <|> P.try pJSPCompLitNum
              <|> P.try pJSPCompSQ

pJSPCompLitString :: P.Parser JSPComparable
pJSPCompLitString = JSPCompLitString . T.pack <$> (P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\'')

pJSPCompLitNum :: P.Parser JSPComparable
pJSPCompLitNum = JSPCompLitNum <$> (P.try pDoubleScientific <|> P.try pScientific)

pJSPCompSQ :: P.Parser JSPComparable
pJSPCompSQ = JSPCompSQ <$> (P.try pJSPRelSingleQ <|> P.try pJSPAbsSingleQ)

pJSPRelSingleQ :: P.Parser JSPSingularQuery
pJSPRelSingleQ = do
  P.char '@'
  segs <- P.many pJSPSingleQSegment
  return $ JSPRelSingleQ segs

pJSPAbsSingleQ :: P.Parser JSPSingularQuery
pJSPAbsSingleQ = do
  P.char '$'
  segs <- P.many pJSPSingleQSegment
  return $ JSPAbsSingleQ segs

pJSPSingleQSegment :: P.Parser JSPSingleQSegment
pJSPSingleQSegment = P.try pJSPSingleQNameSeg <|> P.try pJSPSingleQIndexSeg

pJSPSingleQNameSeg :: P.Parser JSPSingleQSegment
pJSPSingleQNameSeg = P.try pSingleQNameBracketed <|> P.try pSingleQNameDotted
  where
    pSingleQNameBracketed = do
      P.char '['
      name <- T.pack <$> (P.char '\'' *> P.many (P.noneOf "\'") <* P.char '\'')
      P.char ']'
      return $ JSPSingleQNameSeg name

    pSingleQNameDotted = do
      P.char '.'
      P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
      name <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
      return $ JSPSingleQNameSeg name

pJSPSingleQIndexSeg :: P.Parser JSPSingleQSegment
pJSPSingleQIndexSeg = do
  P.char '['
  num <- pSignedInt
  P.char ']'
  return $ JSPSingleQIndexSeg num

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
