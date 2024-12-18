{-# LANGUAGE DeriveGeneric #-}
module QuerySpec
  ( spec )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM

import Data.Aeson.JSONPath (runJSPQuery)
import GHC.Generics (Generic (..))
import Test.Hspec

import Protolude


-- Define a data type for Book
data Book = Book
  { title    :: Text
  , author   :: Text
  , category :: Text
  , price    :: Double
  } deriving (Show, Generic)

instance JSON.ToJSON Book

-- Define a data type for Store containing a list of Books
data Store = Store
  { books :: [Book]
  } deriving (Show, Generic)

instance JSON.ToJSON Store

-- Define a data type for the Root JSON structure containing a Store
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

spec :: Spec
spec = do
  describe "Run JSPQuery on JSON documents" $ do
    it "returns root document when query is $" $
      runJSPQuery "$" rootDoc `shouldBe` Right rootDoc

    it "returns store object when query is $.store" $
      runJSPQuery "$.store" rootDoc `shouldBe` Right storeDoc
