module DumpQuerySpec
  ( spec )
  where

import Test.Hspec

import Data.Aeson.JSONPath  (jsonPath, dumpQuery)

import Prelude

spec :: Spec
spec = do
  describe "test query dump" $ do
    it "should dump the function query" $
      dumpQuery [jsonPath|$.store.books[?search(@.author, 'Diamond|Herman')]|]
      `shouldBe` "$.store.books[?search(@.author, 'Diamond|Herman')]"

    it "should dump filter test query" $
      dumpQuery [jsonPath|$.store.books[?@.not_here]|]
      `shouldBe` "$.store.books[?@.not_here]"

    it "should dump filter comparison query" $
      dumpQuery [jsonPath|$.store.books[?@.price < 20]|]
      `shouldBe` "$.store.books[?@.price < 20.0]"

    it "should dump root" $
      dumpQuery [jsonPath|$|]
      `shouldBe` "$"

    it "should dump dotted segment" $
      dumpQuery [jsonPath|$.store|]
      `shouldBe` "$.store"

    it "should dump bracketed segment" $
      dumpQuery [jsonPath|$['store']|]
      `shouldBe` "$['store']"

    it "should dump dotted segments" $
      dumpQuery [jsonPath|$.store.books|]
      `shouldBe` "$.store.books"

    it "should dump index selector" $
      dumpQuery [jsonPath|$.store.books[0]|]
      `shouldBe` "$.store.books[0]"

    it "should dump -ve index" $
      dumpQuery [jsonPath|$.store.books[-4]|]
      `shouldBe` "$.store.books[-4]"

    it "should dump multiple singular query segment" $
      dumpQuery [jsonPath|$.store.books[0,2]|]
      `shouldBe` "$.store.books[0,2]"

    it "should dump slice" $
      dumpQuery [jsonPath|$.store.books[1:3]|]
      `shouldBe` "$.store.books[1:3:1]"

    it "should dump multiple bracketed" $
      dumpQuery [jsonPath|$.store.books[1:3,0,1]|]
      `shouldBe` "$.store.books[1:3:1,0,1]"

    it "should dump slice with no end" $
      dumpQuery [jsonPath|$[5:]|]
      `shouldBe` "$[5::1]"

    it "should dump slice with start, end and step" $
      dumpQuery [jsonPath|$[1:5:2]|]
      `shouldBe` "$[1:5:2]"

    it "should dump slice with step -2" $
      dumpQuery [jsonPath|$[5:1:-2]|]
      `shouldBe` "$[5:1:-2]"

    it "should dump slice with step -1" $
      dumpQuery [jsonPath|$[::-1]|]
      `shouldBe` "$[::-1]"

    it "should dump child wildcard" $
      dumpQuery [jsonPath|$.*|]
      `shouldBe` "$.*"

    it "should dump segment wildcard" $
      dumpQuery [jsonPath|$[*]|]
      `shouldBe` "$[*]"

    it "should dump descendant wildcard" $
      dumpQuery [jsonPath|$..*|]
      `shouldBe` "$..*"

    it "should dump descendant bracketed" $
      dumpQuery [jsonPath|$..[*]|]
      `shouldBe` "$..[*]"
