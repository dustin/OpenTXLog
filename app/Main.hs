#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

{-# LANGUAGE OverloadedStrings #-}

import OpenTXLog

import Data.Csv (encode)
import qualified Data.ByteString.Lazy as BL

import Options.Applicative (option, auto, long, showDefault, value, help, helper, fullDesc,
                            progDesc, argument, metavar, execParser, info, str, switch,
                            (<**>), Parser)
import Data.Semigroup ((<>))

data Options = Options { optGroundAlt :: Int
                       , optR2D :: Bool
                       , optFilename :: String
                       , optMinSats :: Int
  }

options :: Parser Options
options = Options
  <$> option auto (long "groundAlt" <> showDefault <> value 0 <> help "altitude at takeoff")
  <*> switch (long "r2d" <> showDefault <> help "convert radians to degrees")
  <*> argument str (metavar "FILE")
  <*> option auto (long "minSats" <> showDefault <> value 5 <> help "minimum satellite to consider valid position")

mkTransformers :: Options -> [Transformer]
mkTransformers opts = map snd . filter fst $ [
  (optMinSats opts /= 0, (satFilter . optMinSats) opts),
  (optGroundAlt opts /= 0, simpleTransformer
                           (intFieldTransformer "Alt(m)" (+ optGroundAlt opts))
                           (simpleRenamer "Alt(m)" "GAlt(m)")),
  (optR2D opts, r2dTransformer "Ptch(rad)"),
  (optR2D opts, r2dTransformer "Roll(rad)"),
  (optR2D opts, r2dTransformer "Yaw(rad)"),
  -- Crossfire RSSIs are all logged inverted
  (True, negTrans "1RSS(dB)"),
  (True, negTrans "2RSS(dB)"),
  (True, negTrans "RSNR(dB)"),
  (True, negTrans "TRSS(dB)"),
  (True, negTrans "TSNR(dB)")
  ]

  where negTrans f = simpleTransformer (intFieldTransformer f negate) id

doFile :: Options -> IO ()
doFile opts = do
  stuff <- processCSVFile (optFilename opts) (mkTransformers opts)
  (BL.putStr . encode) stuff

main :: IO ()
main = do
  o <- execParser opts
  doFile o

  where opts = info (options <**> helper) (fullDesc <> progDesc "fix up telemetry.")

