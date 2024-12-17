{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser
  ( JSPQuery (..)
  , JSPSegment (..)
  , JSPChildSegment (..)
  , JSPSelector (..)
  , JSPWildcardT (..)
  , pJSPQuery
  )
  where

import qualified Text.ParserCombinators.Parsec  as P

import Text.Read (read)

import Protolude

data JSPQuery
  = JSPRoot [JSPSegment]
  deriving (Show)

-- https://www.rfc-editor.org/rfc/rfc9535#name-segments-2
data JSPSegment
  = JSPChildSeg JSPChildSegment
  deriving (Show)

-- https://www.rfc-editor.org/rfc/rfc9535#name-child-segment
data JSPChildSegment
  = JSPBracketed [JSPSelector]
  | JSPMemberNameSH JSPNameSelector
  | JSPSegWildcard JSPWildcardT
  deriving (Show)

-- https://www.rfc-editor.org/rfc/rfc9535#name-selectors-2
data JSPSelector
  = JSPNameSel JSPNameSelector
  | JSPIndexSel JSPIndexSelector
  | JSPSliceSel JSPSliceSelector
  | JSPWildSel JSPWildcardT
  deriving (Show)

data JSPWildcardT = JSPWildcard
  deriving (Show)

type JSPNameSelector = Text

type JSPIndexSelector = Int

type JSPSliceSelector = (Maybe Int, Maybe Int, Int)

pJSPQuery :: P.Parser JSPQuery
pJSPQuery = do
  P.char '$'
  segs <- P.many pJSPSegment
  return $ JSPRoot segs

pJSPSelector :: P.Parser JSPSelector
pJSPSelector = P.try pJSPNameSel 
            <|> P.try pJSPIndexSel
            <|> P.try pJSPSliceSel 
            <|> P.try pJSPWildSel

pJSPNameSel :: P.Parser JSPSelector
pJSPNameSel = JSPNameSel <$> toS <$> (P.char '"' *> P.many (P.noneOf "\"") <* P.char '"')

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
pJSPWildSel = JSPWildSel <$> (P.string ".*" $> JSPWildcard)

pJSPSegment :: P.Parser JSPSegment
pJSPSegment = pJSPChildSegment

pJSPChildSegment :: P.Parser JSPSegment
pJSPChildSegment = 
  JSPChildSeg <$> (P.try pJSPBracketed 
                  <|> P.try pJSPMemberNameSH 
                  <|> P.try pJSPWildSeg)

pJSPBracketed :: P.Parser JSPChildSegment
pJSPBracketed =  do
  P.char '['
  sel <- pJSPSelector
  optionalSels <- P.many pCommaSepSelectors
  P.char ']'
  return $ JSPBracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser JSPSelector
      pCommaSepSelectors = P.char ',' *> P.spaces *> pJSPSelector

pJSPMemberNameSH :: P.Parser JSPChildSegment
pJSPMemberNameSH = do
  P.char '.'
  val <- toS <$> P.many1 (P.alphaNum <|> P.oneOf "_$@")
  return (JSPMemberNameSH val)

pJSPWildSeg :: P.Parser JSPChildSegment
pJSPWildSeg = JSPSegWildcard <$> (P.string ".*" $> JSPWildcard)

pSignedInt :: P.Parser Int
pSignedInt = do
  sign <- P.optionMaybe $ P.char '-'
  num <- read <$> P.many1 P.digit
  return $ 
    case sign of
      Just _ -> -1 * num
      Nothing -> num
