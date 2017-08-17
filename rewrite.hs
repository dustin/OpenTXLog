#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

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

type FieldLookup = String -> (V.Vector String) -> String

-- Parse a timestamp to a UTCTime given the timezone and a separate date and time string.
parseTS :: TimeZone -> String -> String -> UTCTime
parseTS tz ds ts =
  let l = defaultTimeLocale
      lt = (LocalTime <$> parseTimeM True l "%F" ds <*> parseTimeM True l "%H:%M:%S%Q" ts) in
    localTimeToUTC tz (lt())

-- Parse the timestamp out of a row.
parseRowTS :: TimeZone -> FieldLookup -> (V.Vector String) -> UTCTime
parseRowTS tz hdr =
  let df = hdr "Date"
      tf = hdr "Time" in
    (\r -> parseTS tz (hdr "Date" r) (hdr "Time" r))

-- Distance (in meters) between two points.
distance (Just a) (Just b) = case groundDistance a b of
                               Nothing -> D._0
                               Just (d, _, _) -> if (isNaN (d D./~ meter)) then D._0 else d

-- Average speed in kph it took to get between two points based on the start and end timestamp.
speed ts1 ts2 pos1 pos2 =
  let tΔ = (realToFrac $ diffUTCTime ts1 ts2) D.*~ second
      pΔ = distance pos1 pos2
      ε = 1 D.*~ second in
    (if tΔ <= ε then D._0 else pΔ D./ tΔ) D./~ (kilo meter D./ hour)

-- Create a FieldLookup function to look up fields in a row by name (based on the header row)
byName :: (V.Vector String) -> FieldLookup
byName hdr field = maybe (\_ -> "") (\n r -> r V.! n) $ V.elemIndex field hdr

-- Drop any records when the GPS position isn't updating.
dropDup _ _ [] rv = reverse rv
dropDup pf prev (x:xs) rv
  | cur == prev = dropDup pf prev xs rv
  | otherwise = dropDup pf cur xs (x : rv)
  where cur = pf x

minDur = 4

-- Remove entries from the head of a list that are within minDur of "now"
prune :: (V.Vector String -> UTCTime) -> FieldLookup -> [V.Vector String] -> (V.Vector String) -> UTCTime -> [V.Vector String]
prune pt hdr vals current now =
  let dt r = diffUTCTime now (pt r) in
    L.dropWhile (\r -> dt r > minDur) vals

-- Add distance and speed columns to telemetry logs.
process :: (V.Vector String -> UTCTime) -> (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process pt hdr vals =
  let pf = byName hdr "GPS"
      home = (readGroundPosition WGS84 $ pf $ head vals)
      (_, vals') = L.mapAccumL (\a r -> let c = readGroundPosition WGS84 $ pf r
                                            d = distance home c
                                            c' = readGroundPosition WGS84 $ pf $ head a
                                            t = pt r
                                            t' = pt $ head a
                                            s = speed t t' c c' in
                                          (prune pt (byName hdr) a r t,
                                           r V.++ V.fromList [show (d D./~ meter), show s]))
                   (dropDup pf "" vals []) vals in
    (hdr V.++ V.fromList ["distance", "speed"]) : vals'

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
