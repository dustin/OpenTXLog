#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

{-# LANGUAGE OverloadedStrings #-}

import OpenTXLog

import Data.Csv (encode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  file <- head <$> getArgs
  stuff <- processCSVFile file []
  (BL.putStr . encode) stuff
