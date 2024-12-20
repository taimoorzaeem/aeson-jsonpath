{-# LANGUAGE DeriveGeneric #-}
module QuerySpec
  ( spec )
  where

import qualified Data.Aeson        as JSON

import Data.Aeson.JSONPath (runJSPQuery)
import Test.Hspec

import Protolude

data Book = Book
  { title    :: Text
  , author   :: Text
  , category :: Text
  , price    :: Double
  } deriving (Show, Generic)

instance JSON.ToJSON Book

data Store = Store
  { books :: [Book]
  } deriving (Show, Generic)

instance JSON.ToJSON Store

data Root = Root
  { store :: Store
  } deriving (Show, Generic)

instance JSON.ToJSON Root

-- taken from https://serdejsonpath.live/
rootDoc :: JSON.Value
rootDoc = JSON.toJSON $ Root
  { store = Store
      { books =
          [ Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
          , Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
          , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
          , Book { title = "Crime and Punishment", author = "Fyodor Dostoevsky", category = "fiction", price = 19.99 }
          ]
      }
  }


storeDoc :: JSON.Value
storeDoc = JSON.toJSON $ Store
    { books =
        [ Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
        , Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
        , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
        , Book { title = "Crime and Punishment", author = "Fyodor Dostoevsky", category = "fiction", price = 19.99 }
        ]
    }


booksDoc :: JSON.Value
booksDoc = JSON.toJSON $
      [ Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
      , Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
      , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
      , Book { title = "Crime and Punishment", author = "Fyodor Dostoevsky", category = "fiction", price = 19.99 }
      ]


books0Doc :: JSON.Value
books0Doc = JSON.toJSON $ [
       Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
    ]

books0And2Doc :: JSON.Value
books0And2Doc = JSON.toJSON $ [
      Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
      , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
    ]

books1To3Doc :: JSON.Value
books1To3Doc = JSON.toJSON $ [
       Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
      , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
    ]

books1To3And0And1Doc :: JSON.Value
books1To3And0And1Doc = JSON.toJSON $ [
       Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
      , Book { title = "Moby Dick", author = "Herman Melville", category = "fiction", price = 8.99 }
      , Book { title = "Guns, Germs, and Steel", author = "Jared Diamond", category = "reference", price = 24.99 }
      , Book { title = "David Copperfield", author = "Charles Dickens", category = "fiction", price = 12.99 }
    ]

data AlphaList = AList [Text]
  deriving (Show, Generic)
instance JSON.ToJSON AlphaList

alphaArr :: JSON.Value
alphaArr = JSON.toJSON $ AList ["a","b","c","d","e","f","g"]

fgArr :: JSON.Value
fgArr = JSON.toJSON $ AList ["f","g"]

bdArr :: JSON.Value
bdArr = JSON.toJSON $ AList ["b","d"]

fdArr :: JSON.Value
fdArr = JSON.toJSON $ AList ["f","d"]

gfedcbaArr :: JSON.Value
gfedcbaArr = JSON.toJSON $ AList ["g","f","e","d","c","b","a"]

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
