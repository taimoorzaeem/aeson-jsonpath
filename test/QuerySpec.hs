module QuerySpec
  ( spec )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Vector       as V

import Data.Aeson.JSONPath  (runJSPQuery, jsonPath)
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Hspec

import Prelude

toSingletonArray :: JSON.Value -> JSON.Value
toSingletonArray = JSON.Array . V.singleton

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

rfcExample1 :: JSON.Value
rfcExample1 = [aesonQQ| {
    "o": {"j": 1, "k": 2},
    "a": [5, 3, [{"j": 4}, {"k": 6}]]
  } |]

rfcExample1Desc :: JSON.Value
rfcExample1Desc = [aesonQQ| [
    [5, 3, [{"j": 4}, {"k": 6}]],
    {"j": 1, "k": 2},
    5,
    3,
    [{"j": 4}, {"k": 6}],
    {"j": 4},
    {"k": 6},
    4,
    6,
    1,
    2
  ]|]

spec :: Spec
spec = do
  describe "Run JSPQuery on JSON documents" $ do
    it "returns root document when query is $" $
      runJSPQuery [jsonPath|$|] rootDoc `shouldBe` rootDoc

    it "returns store object when query is $.store" $
      runJSPQuery [jsonPath|$.store|] rootDoc `shouldBe` storeDoc

    it "returns books array when query is $.store.books" $
      runJSPQuery [jsonPath|$.store.books|] rootDoc `shouldBe` booksDoc

    it "returns 0-index book item when query is $.store.books[0]" $
      runJSPQuery [jsonPath|$.store.books[0]|] rootDoc `shouldBe`  books0Doc

    it "returns 0-index book item when query is $.store.books[-4]" $
      runJSPQuery [jsonPath|$.store.books[-4]|] rootDoc `shouldBe` books0Doc

    it "returns 0,2-index item when query is $.store.books[0,2]" $
      runJSPQuery [jsonPath|$.store.books[0,2]|] rootDoc `shouldBe` books0And2Doc

    it "returns 1To3-index when query is $.store.books[1:3]" $
      runJSPQuery [jsonPath|$.store.books[1:3]|] rootDoc `shouldBe` books1To3Doc

    it "returns 1To3-index and 0,1-index when query is $.store.books[1:3,0,1]" $
      runJSPQuery [jsonPath|$.store.books[1:3,0,1]|] rootDoc `shouldBe` books1To3And0And1Doc

    it "returns slice with query $[5:]" $
      runJSPQuery [jsonPath|$[5:]|] alphaArr `shouldBe` fgArr

    it "returns slice with query $[1:5:2]" $
      runJSPQuery [jsonPath|$[1:5:2]|] alphaArr `shouldBe` bdArr

    it "returns slice with query $[5:1:-2]" $
      runJSPQuery [jsonPath|$[5:1:-2]|] alphaArr `shouldBe` fdArr

    it "returns slice with query $[::-1]" $
      runJSPQuery [jsonPath|$[::-1]|] alphaArr `shouldBe` gfedcbaArr

    it "returns root with query $.*" $
      runJSPQuery [jsonPath|$.*|] rootDoc `shouldBe` rootDoc

    it "returns root with query $[*]" $
      runJSPQuery [jsonPath|$[*]|] rootDoc `shouldBe` (toSingletonArray rootDoc)

    it "returns descendants with query $..*" $
      runJSPQuery [jsonPath|$..*|] rfcExample1 `shouldBe` rfcExample1Desc

    it "returns descendants with query $..[*]" $ do
      pendingWith "fix with wildcard selection"
      runJSPQuery [jsonPath|$..[*]|] rfcExample1 `shouldBe` rfcExample1Desc
