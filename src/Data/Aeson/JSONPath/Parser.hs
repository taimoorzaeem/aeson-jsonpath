{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser
  ( JSPQuery (..)
  , JSPSegment (..)
  , JSPChildSegment (..)
  , JSPDescSegment (..)
  , JSPSelector (..)
  , JSPWildcardT (..)
  , pJSPQuery
  )
  where

import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Functor                  (($>))
import Data.Text                     (Text)
import Data.Char                     (ord)
import Text.ParserCombinators.Parsec ((<|>))
import Language.Haskell.TH.Syntax    (Lift (..))

import Prelude

newtype JSPQuery
  = JSPRoot [JSPSegment]
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-segments-2
data JSPSegment
  = JSPChildSeg JSPChildSegment
  | JSPDescSeg JSPDescSegment
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-child-segment
data JSPChildSegment
  = JSPChildBracketed [JSPSelector]
  | JSPChildMemberNameSH JSPNameSelector
  | JSPChildWildSeg JSPWildcardT
  deriving (Eq, Show, Lift)


-- https://www.rfc-editor.org/rfc/rfc9535#name-descendant-segment
data JSPDescSegment
  = JSPDescBracketed [JSPSelector]
  | JSPDescMemberNameSH JSPNameSelector
  | JSPDescWildSeg JSPWildcardT
  deriving (Eq, Show, Lift)

-- https://www.rfc-editor.org/rfc/rfc9535#name-selectors-2
data JSPSelector
  = JSPNameSel JSPNameSelector
  | JSPIndexSel JSPIndexSelector
  | JSPSliceSel JSPSliceSelector
  | JSPWildSel JSPWildcardT
  deriving (Eq, Show, Lift)

data JSPWildcardT = JSPWildcard
  deriving (Eq, Show, Lift)

type JSPNameSelector = Text

type JSPIndexSelector = Int

type JSPSliceSelector = (Maybe Int, Maybe Int, Int)

pJSPQuery :: P.Parser JSPQuery
pJSPQuery = do
  P.char '$'
  segs <- P.many pJSPSegment
  P.eof
  return $ JSPRoot segs

pJSPSelector :: P.Parser JSPSelector
pJSPSelector = P.try pJSPNameSel 
            <|> P.try pJSPIndexSel
            <|> P.try pJSPSliceSel 
            <|> P.try pJSPWildSel

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
      pCommaSepSelectors = P.char ',' *> P.spaces *> pJSPSelector

pJSPChildMemberNameSH :: P.Parser JSPChildSegment
pJSPChildMemberNameSH = do
  P.char '.'
  P.lookAhead (P.letter <|> P.oneOf "-" <|> pUnicodeChar)
  val <- T.pack <$> P.many1 (P.alphaNum <|> (P.oneOf "-" <|> pUnicodeChar))
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
      pCommaSepSelectors = P.char ',' *> P.spaces *> pJSPSelector

pJSPDescMemberNameSH :: P.Parser JSPDescSegment
pJSPDescMemberNameSH = do
  P.string ".."
  P.lookAhead (P.letter <|> P.oneOf "-" <|> pUnicodeChar)
  val <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "-" <|> pUnicodeChar)
  return (JSPDescMemberNameSH val)

pJSPDescWildSeg :: P.Parser JSPDescSegment
pJSPDescWildSeg = JSPDescWildSeg <$> (P.string "..*" $> JSPWildcard)

pSignedInt :: P.Parser Int
pSignedInt = do
  sign <- P.optionMaybe $ P.char '-'
  num <- read <$> P.many1 P.digit
  return $ 
    case sign of
      Just _ -> -num
      Nothing -> num

pUnicodeChar :: P.Parser Char
pUnicodeChar = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x80 && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)
