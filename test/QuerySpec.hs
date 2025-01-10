module QuerySpec
  ( spec )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Vector       as V

import Data.Aeson.JSONPath  (queryQQ, jsonPath)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Vector          (Vector)
import Test.Hspec

import Prelude

getVector :: JSON.Value -> Vector JSON.Value
getVector (JSON.Array arr) = arr
getVector _ = V.empty

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

lessThanPrice20Books :: JSON.Value
lessThanPrice20Books = [aesonQQ| [
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
      queryQQ [jsonPath|$|] rootDoc `shouldBe` V.singleton rootDoc

    it "returns store object when queryQQ is $.store" $
      queryQQ [jsonPath|$.store|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns store object when queryQQ is $['store']" $
      queryQQ [jsonPath|$['store']|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns books array when queryQQ is $.store.books" $
      queryQQ [jsonPath|$.store.books|] rootDoc `shouldBe` V.singleton booksDoc

    it "returns 0-index book item when queryQQ is $.store.books[0]" $
      queryQQ [jsonPath|$.store.books[0]|] rootDoc `shouldBe` getVector books0Doc

    it "returns 0-index book item when queryQQ is $.store.books[-4]" $
      queryQQ [jsonPath|$.store.books[-4]|] rootDoc `shouldBe` getVector books0Doc

    it "returns 0,2-index item when queryQQ is $.store.books[0,2]" $
      queryQQ [jsonPath|$.store.books[0,2]|] rootDoc `shouldBe` getVector books0And2Doc

    it "returns 1To3-index when queryQQ is $.store.books[1:3]" $
      queryQQ [jsonPath|$.store.books[1:3]|] rootDoc `shouldBe` getVector books1To3Doc

    it "returns 1To3-index and 0,1-index when queryQQ is $.store.books[1:3,0,1]" $
      queryQQ [jsonPath|$.store.books[1:3,0,1]|] rootDoc `shouldBe` getVector books1To3And0And1Doc

    it "returns slice with queryQQ $[5:]" $
      queryQQ [jsonPath|$[5:]|] alphaArr `shouldBe` getVector fgArr

    it "returns slice with queryQQ $[1:5:2]" $
      queryQQ [jsonPath|$[1:5:2]|] alphaArr `shouldBe` getVector bdArr

    it "returns slice with queryQQ $[5:1:-2]" $
      queryQQ [jsonPath|$[5:1:-2]|] alphaArr `shouldBe` getVector fdArr

    it "returns slice with queryQQ $[::-1]" $
      queryQQ [jsonPath|$[::-1]|] alphaArr `shouldBe` getVector gfedcbaArr

    it "returns root with queryQQ $.*" $
      queryQQ [jsonPath|$.*|] rootDoc `shouldBe` V.singleton rootDoc

    it "returns root with queryQQ $[*]" $
      queryQQ [jsonPath|$[*]|] rootDoc `shouldBe` V.singleton rootDoc

    it "returns descendants with queryQQ $..*" $
      queryQQ [jsonPath|$..*|] rfcExample1 `shouldBe` getVector rfcExample1Desc

    it "returns descendants with queryQQ $..[*]" $
      queryQQ [jsonPath|$..[*]|] rfcExample1 `shouldBe` getVector rfcExample1Desc

    it "returns with filtering: number comparison" $
      queryQQ [jsonPath| $.store.books[?@.price < 20] |] rootDoc
      `shouldBe` getVector lessThanPrice20Books

    it "returns with filtering: not operator" $
      queryQQ [jsonPath| $.store.books[?!(@.price < 20)] |] rootDoc
      `shouldBe` getVector books0Doc

    it "returns with filtering: string comparison" $
      queryQQ [jsonPath| $.store.books[?@.author == 'Jared Diamond'] |] rootDoc
      `shouldBe` getVector books0Doc

    it "returns with filtering: test expression" $
      queryQQ [jsonPath| $.store.books[?@.author] |] rootDoc
      `shouldBe` getVector booksDoc

    it "returns with filtering: test expr gives empty with non-existent key" $
      queryQQ [jsonPath| $.store.books[?@.not_here] |] rootDoc
      `shouldBe` V.empty
