#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

import OpenTXLog

import Data.Time
import Data.Csv
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  file <- head <$> getArgs
  csvData <- BL.readFile file
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
    Left err -> putStrLn err
    Right v ->
      let hdr = V.head v
          body = V.toList $ V.tail v in
        BL.putStr $ encode $ process (parseRowTS tz $ byName hdr) hdr body
