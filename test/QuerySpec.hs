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


spec :: Spec
spec = do
  describe "Run JSPQuery on JSON documents" $ do
    it "returns root document when query is $" $
      runJSPQuery "$" rootDoc `shouldBe` Right rootDoc

    it "returns store object when query is $.store" $
      runJSPQuery "$.store" rootDoc `shouldBe` Right storeDoc

    it "returns books array when query is $.store.books" $
      runJSPQuery "$.store.books" rootDoc `shouldBe` Right booksDoc

    it "returns first book item when query is $.store.books[0]" $
      runJSPQuery "$.store.books[0]" rootDoc `shouldBe` Right books0Doc

    it "returns first and third item with query is $.store.books[0,2]" $
      runJSPQuery "$.store.books[0,2]" rootDoc `shouldBe` Right books0And2Doc
