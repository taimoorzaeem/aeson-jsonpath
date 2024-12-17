module Main
  ( main )
  where

import qualified Test.Hspec        as HS
import Test.Hspec.Runner

import qualified ParserSpec

import System.Exit (exitFailure, exitSuccess)
import Protolude

allSpecs :: Spec
allSpecs = do
  HS.describe "Run all tests" $
    ParserSpec.spec

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto     -- Colorized output
    } allSpecs
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: " ++ show (summaryFailures summary)
