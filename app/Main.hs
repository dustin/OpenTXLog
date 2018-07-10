#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

{-# LANGUAGE OverloadedStrings #-}

import OpenTXLog

import Data.Csv (encode)
import qualified Data.ByteString.Lazy as BL

import Options.Applicative (option, auto, long, showDefault, value, help, helper, fullDesc,
                            progDesc, argument, metavar, execParser, info, str,
                            (<**>), Parser)
import Data.Semigroup ((<>))

data Options = Options { optGroundAlt :: Int
                       , optFilename :: String
  }

options :: Parser Options
options = Options
  <$> option auto (long "groundAlt" <> showDefault <> value 0 <> help "altitude at takeoff")
  <*> argument str (metavar "FILE")

mkTransformers :: Options -> [Transformer]
mkTransformers opts = map snd . filter fst $ [
  (optGroundAlt opts /= 0, intFieldTransformer "Alt(m)" (+ (optGroundAlt opts)))
  ]

doFile :: Options -> IO ()
doFile opts = do
  stuff <- processCSVFile (optFilename opts) (mkTransformers opts)
  (BL.putStr . encode) stuff

main :: IO ()
main = do
  o <- execParser opts
  doFile o

  where opts = info (options <**> helper) (fullDesc <> progDesc "fix up telemetry.")

