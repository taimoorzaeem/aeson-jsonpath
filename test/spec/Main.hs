module Main
  ( main )
  where

import qualified Test.Hspec as HS

import qualified ParserSpec
import qualified QuerySpec
import qualified LocatedSpec
import qualified DumpQuerySpec

import Test.Hspec.Runner
import Prelude

specs :: Spec
specs = do
  HS.describe "Run all tests" $ do
    ParserSpec.spec
    QuerySpec.spec
    LocatedSpec.spec
    DumpQuerySpec.spec


main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto
    } specs
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: "    ++ show (summaryFailures summary)
