module Main
  ( main )
  where

import qualified Data.Aeson         as JSON
import qualified Test.Hspec         as HS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as TIO

import Data.Either                  (fromRight)

import qualified ParserSpec
import qualified QuerySpec
import qualified ComplianceSpec
import qualified Paths_aeson_jsonpath as Paths

import ComplianceSpec   (TestSuite (..))

import Test.Hspec.Runner
import Prelude

allSpecs :: Either String TestSuite -> Spec
allSpecs cts = do
  HS.describe "Run all tests" $ do
    ParserSpec.spec
    QuerySpec.spec
    ComplianceSpec.spec (fromRight TestSuite{tests=[]} cts)

readCtsFile :: FilePath -> IO (Either String TestSuite)
readCtsFile filePath = do
  contents <- TIO.readFile filePath
  return $ JSON.eitherDecodeStrict' $ T.encodeUtf8 contents

main :: IO ()
main = do
  file <- Paths.getDataFileName "test/cts.json"
  cts <- readCtsFile file
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto
    } (allSpecs cts)
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: "    ++ show (summaryFailures summary)
