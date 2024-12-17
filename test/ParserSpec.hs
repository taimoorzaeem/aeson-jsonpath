module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P
import Test.Hspec

import Data.Aeson.JSONPath.Parser (pJSPQuery, JSPQuery (..))
import Protolude

spec :: Spec
spec = do
  describe "Parse JSPQuery" $ do
    it "parses JSPQuery with only $ sign" $
      P.parse pJSPQuery "" "$" `shouldBe` Right (JSPRoot [])
