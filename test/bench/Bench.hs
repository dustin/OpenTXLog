{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Criterion
import Criterion.Main

import OpenTXLog

main :: IO ()
main = defaultMain [
  bench "sample" $ nfIO (processCSVFile "test/sample.csv")
  ]
