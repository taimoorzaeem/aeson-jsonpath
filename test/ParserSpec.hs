module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P
import Test.Hspec

import Data.Aeson.JSONPath.Parser (pJSPQuery
                                  , JSPQuery (..)
                                  , JSPSegment (..)
                                  , JSPChildSegment (..)
                                  , JSPDescSegment (..)
                                  , JSPSelector (..)
                                  , JSPWildcardT (..))
import Prelude

spec :: Spec
spec = do
  describe "Parse JSPQuery" $ do
    it "parses query: $" $
      P.parse pJSPQuery "" "$" `shouldBe` Right (JSPRoot [])

    it "parses query: $.store" $
      P.parse pJSPQuery "" "$.store" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store")])

    it "parses query: $.store.books" $
      P.parse pJSPQuery "" "$.store.books" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books")])

    it "parses query: $.store.books[0]" $
      P.parse pJSPQuery "" "$.store.books[0]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPIndexSel 0])])

    it "parses query: $.store.books[0,2]" $
      P.parse pJSPQuery "" "$.store.books[0,2]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPIndexSel 0, JSPIndexSel 2])])

    it "parses query: $.store.books[1:3]" $
      P.parse pJSPQuery "" "$.store.books[1:3]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPSliceSel (Just 1, Just 3, 1)])])

    it "parses query: $.store.books[1:4:2]" $
      P.parse pJSPQuery "" "$.store.books[1:4:2]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPSliceSel (Just 1, Just 4, 2)])])

    it "parses query: $.store.books[-1:-4:-2]" $
      P.parse pJSPQuery "" "$.store.books[-1:-4:-2]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPSliceSel (Just (-1), Just (-4), -2)])])

    it "parses query: $.store.books[1:3, 0, 1]" $
      P.parse pJSPQuery "" "$.store.books[1:3, 0, 1]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPSliceSel (Just 1, Just 3, 1), JSPIndexSel 0, JSPIndexSel 1])])

    it "parses query: $.*" $
      P.parse pJSPQuery "" "$.*" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildWildSeg JSPWildcard)])

    it "parses query: $[*]" $
      P.parse pJSPQuery "" "$[*]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildBracketed [JSPWildSel JSPWildcard])])

    it "parses query: $..*" $
      P.parse pJSPQuery "" "$..*" `shouldBe` Right (JSPRoot [JSPDescSeg (JSPDescWildSeg JSPWildcard)])

    it "parses query: $..[*]" $
      P.parse pJSPQuery "" "$..[*]" `shouldBe` Right (JSPRoot [JSPDescSeg (JSPDescBracketed [JSPWildSel JSPWildcard])])
