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

type FieldLookup = String -> (V.Vector String) -> String

distance (Just a) (Just b) = case groundDistance a b of
                               Nothing -> D._0
                               Just (d, _, _) -> if (isNaN (d D./~ meter)) then D._0 else d


parseTS tz ds ts =
  let l = defaultTimeLocale
      lt = (LocalTime <$> parseTimeM True l "%F" ds <*> parseTimeM True l "%H:%M:%S%Q" ts) in
    localTimeToUTC tz (lt())

parseRowTS :: TimeZone -> FieldLookup -> (V.Vector String) -> UTCTime
parseRowTS tz hdr =
  let df = hdr "Date"
      tf = hdr "Time" in
    (\r -> parseTS tz (hdr "Date" r) (hdr "Time" r))

diffTime t1 t2 = (realToFrac $ diffUTCTime t1 t2) D.*~ second

kph = (kilo meter D./ hour)

speed ts1 ts2 pos1 pos2 =
  ((distance pos1 pos2) D./ (diffTime ts1 ts2)) D./~ kph


byName :: (V.Vector String) -> FieldLookup
byName hdr field = case V.elemIndex field hdr of
                     Nothing -> (\_ -> "")
                     Just n -> (\r -> r V.! n)

minDur = 4

-- Trailing edge for computing speed
prune :: (V.Vector String -> UTCTime) -> FieldLookup -> [V.Vector String] -> (V.Vector String) -> UTCTime -> [V.Vector String]
prune pt hdr vals current now
  | pos == (pf $ head vals) = vals
  | otherwise =
      let dt r = diffUTCTime now (pt r) in
        L.dropWhile (\r -> dt r > minDur) vals
  where pf = hdr "GPS"
        pos = (pf current)

process :: (V.Vector String -> UTCTime) -> (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process pt hdr vals =
  let pf = byName hdr "GPS"
      home = (readGroundPosition WGS84 $ pf $ head vals)
      -- Don't advance a if the position isn't changing
      (_, vals') = L.mapAccumL (\a r -> let c = readGroundPosition WGS84 $ pf r
                                            d = distance home c
                                            c' = readGroundPosition WGS84 $ pf $ head a
                                            t = pt r
                                            t' = pt $ head a
                                            s = speed t t' c c' in
                                          (prune pt (byName hdr) a r t,
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
      let hdr = V.head v
          body = V.toList $ V.tail v in
        BL.putStr $ encode $ process (parseRowTS tz $ byName hdr) hdr body
