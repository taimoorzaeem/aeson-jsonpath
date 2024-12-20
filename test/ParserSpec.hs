module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P
import Test.Hspec

import Data.Aeson.JSONPath.Parser (pJSPQuery
                                  , JSPQuery (..)
                                  , JSPSegment (..)
                                  , JSPChildSegment (..)
                                  , JSPSelector (..))
import Protolude

spec :: Spec
spec = do
  describe "Parse JSPQuery" $ do
    it "parses JSPQuery with query: $" $
      P.parse pJSPQuery "" "$" `shouldBe` Right (JSPRoot [])

    it "parses JSPQuery with query: $.store" $
      P.parse pJSPQuery "" "$.store" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPMemberNameSH "store")])

    it "parses JSPQuery with query: $.store.books" $
      P.parse pJSPQuery "" "$.store.books" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPMemberNameSH "store"), JSPChildSeg (JSPMemberNameSH "books")])

    it "parses JSPQuery with query: $.store.books[0]" $
      P.parse pJSPQuery "" "$.store.books[0]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPMemberNameSH "store"), JSPChildSeg (JSPMemberNameSH "books"), JSPChildSeg (JSPBracketed [JSPIndexSel 0])])
