{- |
Module      : Data.Aeson.JSONPath.Parser.Common
Description :
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
module Data.Aeson.JSONPath.Parser.Common
  ( pSpaces
  , pUnicodeChar
  )
  where

import qualified Text.ParserCombinators.Parsec as P
import           Data.Char (ord)

import Prelude

-- https://www.rfc-editor.org/rfc/rfc9535#name-syntax
pSpaces :: P.Parser [Char]
pSpaces = P.many (P.oneOf " \n\r\t")

pUnicodeChar :: P.Parser Char
pUnicodeChar = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x80 && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)
