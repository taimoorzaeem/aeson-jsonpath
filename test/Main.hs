module Main
  ( main )
  where

import qualified Test.Hspec        as HS
import Test.Hspec.Runner

import qualified ParserSpec
import qualified QuerySpec

import Prelude

allSpecs :: Spec
allSpecs = do
  HS.describe "Run all tests" $ do
    ParserSpec.spec
    QuerySpec.spec

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto     -- Colorized output
    } allSpecs
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: " ++ show (summaryFailures summary)
