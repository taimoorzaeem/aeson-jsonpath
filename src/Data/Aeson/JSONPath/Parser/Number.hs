{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser.Number
  ( pSignedInt
  , pScientific
  , pDoubleScientific
  )
  where

import qualified Text.ParserCombinators.Parsec  as P

import Data.Maybe                    (fromMaybe)
import Data.Scientific               (Scientific, scientific)
import GHC.Num                       (integerFromInt, integerToInt)
import Text.ParserCombinators.Parsec ((<?>))

import Prelude

pSignedInt :: P.Parser Int
pSignedInt = do
  P.notFollowedBy (P.string "-0" *> P.optional P.digit) -- no leading -011... etc
  P.notFollowedBy (P.char   '0' *> P.digit) -- no leading 011... etc
  sign <- P.optionMaybe $ P.char '-'
  num <- (read <$> P.many1 P.digit) :: P.Parser Integer
  checkNumOutOfRange num sign <?> "signed integer"
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
  return (scientific (integerFromInt mantissa) (fromMaybe 0 expo)) <?> "scientific integer"

pDoubleScientific :: P.Parser Scientific
pDoubleScientific = do
  whole <- P.many1 P.digit
  P.char '.'
  frac <- P.many1 P.digit
  expo <- P.optionMaybe (P.oneOf "eE" *> pExponent)
  let num = read (whole ++ "." ++ frac ++ maybe "" (\x -> "e" ++ show x) expo) :: Scientific
  return num <?> "scientific double"

pExponent :: P.Parser Int
pExponent = do
  sign <- P.optionMaybe (P.oneOf "+-")
  num <- read <$> P.many1 P.digit
  let e = case sign of {Just '-' -> -num; _ -> num }
  return e <?> "exponent"
