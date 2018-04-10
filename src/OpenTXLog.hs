module OpenTXLog (
  FieldLookup
  , process
  , processCSVFile
  , parseRowTS
  , byName
  , dropDup
  ) where

import Data.Csv (HasHeader(..), decode)
import Data.Function (on)
import Data.Semigroup ((<>))
import Data.Time
import Geodetics.Ellipsoids (Ellipsoid)
import Geodetics.Geodetic (Geodetic(..), WGS84(..), readGroundPosition, groundDistance)
import Numeric.Units.Dimensional.SIUnits
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Numeric.Units.Dimensional as D

type FieldLookup = String -> V.Vector String -> String


-- Parse a timestamp to a UTCTime given the timezone and a separate date and time string.
parseTS :: TimeZone -> String -> String -> UTCTime
parseTS tz ds ts =
  let l = defaultTimeLocale
      lt = (LocalTime <$> parseTimeM True l "%F" ds <*> parseTimeM True l "%H:%M:%S%Q" ts) in
    localTimeToUTC tz (lt())

-- Parse the timestamp out of a row.
parseRowTS :: TimeZone -> FieldLookup -> V.Vector String -> UTCTime
parseRowTS tz hdr r = parseTS tz (hdr "Date" r) (hdr "Time" r)

-- Distance (in meters) between two points.
distance :: Ellipsoid e => Maybe (Geodetic e) -> Maybe (Geodetic e) -> D.Quantity D.DLength Double
distance a b = case groundDistance <$> a <*> b of
                 Just (Just (d, _, _)) -> if isNaN (d D./~ meter) then D._0 else d
                 _ -> D._0

-- Average speed in kph it took to get between two points based on the start and end timestamp.
speed :: Ellipsoid e => UTCTime -> UTCTime -> Maybe (Geodetic e) -> Maybe (Geodetic e) -> Double
speed ts1 ts2 pos1 pos2 =
  let tΔ = realToFrac (diffUTCTime ts1 ts2) D.*~ second
      pΔ = distance pos1 pos2
      ε = 1 D.*~ second in
    (if tΔ <= ε then D._0 else pΔ D./ tΔ) D./~ (kilo meter D./ hour)

-- Create a FieldLookup function to look up fields in a row by name (based on the header row)
byName :: V.Vector String -> FieldLookup
byName hdr field = maybe (const "") (flip (V.!)) $ V.elemIndex field hdr

dropDup :: Eq a => (t -> a) -> [t] -> [t]
dropDup _ [] = []
dropDup pf (x:xs) = x:dropDup pf (dropWhile (on (==) pf x) xs)

minDur :: NominalDiffTime
minDur = 4

-- Remove entries from the head of a list that are within minDur of "now"
prune :: (V.Vector String -> UTCTime)  -> [V.Vector String] -> UTCTime -> [V.Vector String]
prune pt vals now =
  let dt r = diffUTCTime now (pt r) in
    L.dropWhile ((> minDur) . dt) vals


-- Add distance and speed columns to telemetry logs.
process :: (V.Vector String -> UTCTime) -> V.Vector String -> [V.Vector String] -> [V.Vector String]
process pt hdr vals =
  let pf = byName hdr "GPS"
      rgp = readGroundPosition WGS84 . pf
      home = foldr (\x o -> if pf x == "" then o else rgp x) Nothing vals
      (_, vals') = L.mapAccumL (\a r -> let c = rgp r
                                            d = distance home c
                                            c' = rgp $ head a
                                            t = pt r
                                            t' = pt $ head a
                                            s = speed t t' c c' in
                                          (prune pt a t,
                                           r <> V.fromList [show (d D./~ meter), show s]))
                   (dropDup pf vals) vals in
    (hdr <> V.fromList ["distance", "speed"]) : vals'

processCSVFile :: String -> IO [V.Vector String]
processCSVFile file = do
  tz <- getCurrentTimeZone
  csvData <- BL.readFile file
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
    Left err -> fail (show err)
    Right v ->
      let hdr = V.head v
          body = V.toList $ V.tail v in
        pure $ process (parseRowTS tz $ byName hdr) hdr body
