#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

import Control.Exception.Base
import Data.Csv
import Data.Time
import Data.Time.LocalTime
import Geodetics.Geodetic
import Numeric.Units.Dimensional.SIUnits
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Numeric.Units.Dimensional as D

distance (Just a) (Just b) = case groundDistance a b of
                               Nothing -> D._0
                               Just (d, _, _) -> if (isNaN (d D./~ meter)) then D._0 else d


parseTS ds ts tz =
  let l = defaultTimeLocale
      lt = (LocalTime <$> parseTimeM True l "%F" ds <*> parseTimeM True l "%H:%M:%S%Q" ts) in
    localTimeToUTC tz (lt())

diffTime t1 t2 = (realToFrac $ diffUTCTime t1 t2) D.*~ second

kph = (kilo meter D./ hour)

speed ts1 ts2 pos1 pos2 =
  ((distance pos1 pos2) D./ (diffTime ts1 ts2)) D./~ kph


byName :: String -> (V.Vector String) -> (V.Vector String) -> String
byName field hdr = case V.elemIndex field hdr of
                     Nothing -> (\_ -> "")
                     Just n -> (\r -> r V.! n)

minDur = 4

-- Trailing edge for computing speed
prune :: TimeZone -> (V.Vector String) -> [V.Vector String] -> (V.Vector String) -> UTCTime -> [V.Vector String]
prune tz hdr vals current now
  | pos == (pf $ head vals) = vals
  | otherwise =
      let dt r = diffUTCTime now (parseTS (df r) (tf r) tz) in
        L.dropWhile (\r -> dt r > minDur) vals
  where pf = byName "GPS" hdr
        df = byName "Date" hdr
        tf = byName "Time" hdr
        pos = (pf current)

process :: TimeZone -> (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process tz hdr vals =
  let pf = byName "GPS" hdr
      df = byName "Date" hdr
      tf = byName "Time" hdr
      home = (readGroundPosition WGS84 $ pf $ head vals)
      -- Don't advance a if the position isn't changing
      (_, vals') = L.mapAccumL (\a r -> let c = readGroundPosition WGS84 $ pf r
                                            d = distance home c
                                            c' = readGroundPosition WGS84 $ pf $ head a
                                            t = parseTS (df r) (tf r) tz
                                            t' = parseTS (df $ head a) (tf $ head a) tz
                                            s = speed t t' c c' in
                                          (prune tz hdr a r t,
                                           r V.++ V.fromList [show (d D./~ meter), show s]))
                   vals vals in
    (hdr V.++ V.fromList ["distance", "speed"]) : vals'

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  file <- fmap head getArgs
  csvData <- BL.readFile file
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
    Left err -> putStrLn err
    Right v ->
      BL.putStr $ encode $ process tz (V.head v) $ V.toList $ V.tail v
