module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P
import Test.Hspec

import Data.Aeson.JSONPath.Parser (pJSPQuery)
import Data.Aeson.JSONPath.Types  (JSPQuery (..)
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
import Data.Either                (isLeft)
import Prelude

spec :: Spec
spec = do
  describe "Parse JSPQuery" $ do
    it "parses query: $" $
      P.parse pJSPQuery "" "$" `shouldBe` Right (JSPRoot [])

    it "parses query: $.store" $
      P.parse pJSPQuery "" "$.store" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store")])

    it "parses query: $['store']" $
      P.parse pJSPQuery "" "$['store']" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildBracketed [JSPNameSel "store"])])

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

    it "parses query $.hyphen-key" $
      P.parse pJSPQuery "" "$.hyphen-key" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "hyphen-key")])

    it "fails with $.1startsWithNum" $
      P.parse pJSPQuery "" "$.1startsWithNum" `shouldSatisfy` isLeft

    it "parses query $.©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ" $
      P.parse pJSPQuery "" "$.©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ")])

    it "parses query with spaces around" $
      P.parse pJSPQuery "" "  $.key  " `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "key")])

    describe "parses JSPFilter Query" $ do
      it "$..books[?@.category == 'reference'].*" $
        P.parse pJSPQuery "" "$..books[?@.category == 'reference'].*" `shouldBe` Right (JSPRoot [JSPDescSeg (JSPDescMemberNameSH "books"),JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "category"])) JSPEqual (JSPCompLitString "reference"))]])]),JSPChildSeg (JSPChildWildSeg JSPWildcard)])

      it "test expr: $..books[?@.price].title" $
        P.parse pJSPQuery "" "$..books[?@.price].title" `shouldBe` Right (JSPRoot [JSPDescSeg (JSPDescMemberNameSH "books"),JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPTest (JSPTestTrue (JSPFilterRelQ [JSPChildSeg (JSPChildMemberNameSH "price")]))]])]),JSPChildSeg (JSPChildMemberNameSH "title")])

      it "and expr: $.store.books[?@.price < 20 && @.price > 10]" $
        P.parse pJSPQuery "" "$.store.books[?@.price < 20 && @.price > 10]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"),JSPChildSeg (JSPChildMemberNameSH "books"),JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPGreater (JSPCompLitNum 20.0)),JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPLess (JSPCompLitNum 10.0))]])])])

      it "or expr: $.store.books[?@.price < 20 || @.price > 10]" $
        P.parse pJSPQuery "" "$.store.books[?@.price < 20 || @.price > 10]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"),JSPChildSeg (JSPChildMemberNameSH "books"),JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPGreater (JSPCompLitNum 20.0))],JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPLess (JSPCompLitNum 10.0))]])])])

      it "root filter: $.store.books[?$.price].title" $
        P.parse pJSPQuery "" "$.store.books[?$.price].title" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPTest (JSPTestTrue (JSPFilterRootQ [JSPChildSeg (JSPChildMemberNameSH "price")]))]])]), JSPChildSeg (JSPChildMemberNameSH "title")])

      it "not expr: $.store.books[?!(@.price < 20 && @.price > 10)]" $
        P.parse pJSPQuery "" "$.store.books[?!(@.price < 20 && @.price > 10)]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"),JSPChildSeg (JSPChildMemberNameSH "books"),JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPParen (JSPParenFalse (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPGreater (JSPCompLitNum 20.0)),JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPLess (JSPCompLitNum 10.0))]]))]])])])

      it "scientific: $.store.books[?@.price < -1e20]" $
        P.parse pJSPQuery "" "$.store.books[?@['price'] < -1e20]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPGreater (JSPCompLitNum (-1.0e20)))]])])])

      it "double: $.store.books[?@.price < 0.01]" $
        P.parse pJSPQuery "" "$.store.books[?@['price'] < 0.01]" `shouldBe` Right (JSPRoot [JSPChildSeg (JSPChildMemberNameSH "store"), JSPChildSeg (JSPChildMemberNameSH "books"), JSPChildSeg (JSPChildBracketed [JSPFilterSel (JSPLogicalOr [JSPLogicalAnd [JSPComparison (JSPComp (JSPCompSQ (JSPRelSingleQ [JSPSingleQNameSeg "price"])) JSPGreater (JSPCompLitNum 0.01))]])])])
