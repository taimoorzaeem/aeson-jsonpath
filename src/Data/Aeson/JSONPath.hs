module Data.Aeson.JSONPath
  ( query
  , jsonPath)
  where

import qualified Text.ParserCombinators.Parsec as P

import Data.Aeson                   (Value)
import Data.Vector                  (Vector)
import Language.Haskell.TH.Quote    (QuasiQuoter (..))
import Language.Haskell.TH.Syntax   (lift)

import Data.Aeson.JSONPath.Query.Types (Query (..)
                                       , QuerySegment (..)
                                       , Segment (..)
                                       , Selector (..))
import Data.Aeson.JSONPath.Query       (qQuery
                                       , qQuerySegment
                                       , qSegment
                                       , qSelector)
import Data.Aeson.JSONPath.Parser      (pQuery)

import Prelude


jsonPath :: QuasiQuoter
jsonPath = QuasiQuoter
  { quoteExp = \q -> case P.parse pQuery ("failed to parse query: " <> q) q of
      Left err -> fail $ show err
      Right ex -> lift ex
  , quotePat = error "Error: quotePat"
  , quoteType = error "Error: quoteType"
  , quoteDec = error "Error: quoteDec"
  }

query :: Query -> Value -> Vector Value
query q root = query' q root root

class Queryable a where
  query' :: a -> Value -> Value -> Vector Value

instance Queryable Query where
  query' = qQuery

instance Queryable QuerySegment where
  query' = qQuerySegment

instance Queryable Segment where
  query' = qSegment

instance Queryable Selector where
  query' = qSelector
