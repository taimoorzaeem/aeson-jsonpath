module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P
import Test.Hspec

import Data.Aeson.JSONPath.Parser (pJSPQuery
                                  , JSPQuery (..)
                                  , JSPSegment (..)
                                  , JSPChildSegment (..))
import Protolude

spec :: Spec
spec = do
  describe "Parse JSPQuery" $ do
    it "parses JSPQuery with query: $" $
      P.parse pJSPQuery "" "$" `shouldBe` Right (JSPRoot [])

    it "parses JSPQuery with query: $.store" $
      P.parse pJSPQuery "" "$.store" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPMemberNameSH "store")])
