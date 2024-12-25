{-# LANGUAGE QuasiQuotes #-}
module QuerySpec
  ( spec )
  where

import qualified Data.Aeson        as JSON

import Data.Aeson.JSONPath  (runJSPQuery)
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Hspec

import Prelude

-- taken from https://serdejsonpath.live/
rootDoc :: JSON.Value
rootDoc = [aesonQQ|{
    "store": {
      "books": [
        {
          "title": "Guns, Germs, and Steel",
          "author": "Jared Diamond",
          "category": "reference",
          "price": 24.99
        },
        {
          "title": "David Copperfield",
          "author": "Charles Dickens",
          "category": "fiction",
          "price": 12.99
        },
        {
          "title": "Moby Dick",
          "author": "Herman Melville",
          "category": "fiction",
          "price": 8.99
        },
        {
          "title": "Crime and Punishment",
          "author": "Fyodor Dostoevsky",
          "category": "fiction",
          "price": 19.99
        }
      ]
    }
  }|]


storeDoc :: JSON.Value
storeDoc = [aesonQQ|
  {
    "books": [
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      },
      {
        "title": "Crime and Punishment",
        "author": "Fyodor Dostoevsky",
        "category": "fiction",
        "price": 19.99
      }
    ]
  }|]

booksDoc :: JSON.Value
booksDoc = [aesonQQ| [
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      },
      {
        "title": "Crime and Punishment",
        "author": "Fyodor Dostoevsky",
        "category": "fiction",
        "price": 19.99
      }
  ]|]


books0Doc :: JSON.Value
books0Doc = [aesonQQ|[
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      }
  ]|]

books0And2Doc :: JSON.Value
books0And2Doc = [aesonQQ|[
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      }
  ]|]

books1To3Doc :: JSON.Value
books1To3Doc = [aesonQQ|[
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      }
  ]|]

books1To3And0And1Doc :: JSON.Value
books1To3And0And1Doc = [aesonQQ|[
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      },
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      }
  ]|]


alphaArr :: JSON.Value
alphaArr = [aesonQQ| ["a","b","c","d","e","f","g"] |]

fgArr :: JSON.Value
fgArr = [aesonQQ| ["f","g"] |]

bdArr :: JSON.Value
bdArr = [aesonQQ| ["b","d"] |]

fdArr :: JSON.Value
fdArr = [aesonQQ| ["f","d"] |]

gfedcbaArr :: JSON.Value
gfedcbaArr = [aesonQQ| ["g","f","e","d","c","b","a"] |]

spec :: Spec
spec = do
  describe "Run JSPQuery on JSON documents" $ do
    it "returns root document when query is $" $
      runJSPQuery "$" rootDoc `shouldBe` Right rootDoc

    it "returns store object when query is $.store" $
      runJSPQuery "$.store" rootDoc `shouldBe` Right storeDoc

    it "returns books array when query is $.store.books" $
      runJSPQuery "$.store.books" rootDoc `shouldBe` Right booksDoc

    it "returns 0-index book item when query is $.store.books[0]" $
      runJSPQuery "$.store.books[0]" rootDoc `shouldBe` Right books0Doc

    it "returns 0-index book item when query is $.store.books[-4]" $
      runJSPQuery "$.store.books[-4]" rootDoc `shouldBe` Right books0Doc

    it "returns 0,2-index item when query is $.store.books[0,2]" $
      runJSPQuery "$.store.books[0,2]" rootDoc `shouldBe` Right books0And2Doc

    it "returns 1To3-index when query is $.store.books[1:3]" $
      runJSPQuery "$.store.books[1:3]" rootDoc `shouldBe` Right books1To3Doc

    it "returns 1To3-index and 0,1-index when query is $.store.books[1:3,0,1]" $
      runJSPQuery "$.store.books[1:3,0,1]" rootDoc `shouldBe` Right books1To3And0And1Doc

    it "returns slice with query $[5:]" $
      runJSPQuery "$[5:]" alphaArr `shouldBe` Right fgArr

    it "returns slice with query $[1:5:2]" $
      runJSPQuery "$[1:5:2]" alphaArr `shouldBe` Right bdArr

    it "returns slice with query $[5:1:-2]" $
      runJSPQuery "$[5:1:-2]" alphaArr `shouldBe` Right fdArr

    it "returns slice with query $[::-1]" $
      runJSPQuery "$[::-1]" alphaArr `shouldBe` Right gfedcbaArr
