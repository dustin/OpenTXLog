module Main (main) where

import Criterion (bench, nfIO)
import Criterion.Main (defaultMain)

import OpenTXLog (processCSVFile)

main :: IO ()
main = defaultMain [
  bench "sample" $ nfIO (processCSVFile "test/sample.csv")
  ]
