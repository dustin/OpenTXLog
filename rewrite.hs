#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics --package time

import Debug.Trace
import Data.Csv
import Control.Exception.Base
import Geodetics.Geodetic
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional.SIUnits
import System.Environment
import Data.Time.LocalTime
import Data.Time
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V

distance (Just a) (Just b) = case groundDistance a b of
                               Nothing -> D._0
                               Just (d, _, _) -> if (isNaN (d D./~ meter)) then D._0 else d


parseTS ds ts tz =
  let lt = (LocalTime <$> parseTimeM True defaultTimeLocale "%F" ds <*> parseTimeM True defaultTimeLocale "%H:%M:%s%Q" ts) in
    localTimeToUTC tz (lt())

diffTime t1 t2 = abs (realToFrac $ diffUTCTime t1 t2) D.*~ second

kph = (kilo meter D./ hour)

speed ts1 ts2 pos1 pos2 =
  ((distance pos1 pos2) D./ (diffTime ts1 ts2)) D./~ kph


byName :: String -> (V.Vector String) -> (V.Vector String) -> String
byName field hdr = case V.elemIndex field hdr of
                     Nothing -> (\_ -> "")
                     Just n -> (\r -> r V.! n)

process :: TimeZone -> (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process tz hdr vals =
  let pf = byName "GPS" hdr
      df = byName "Date" hdr
      tf = byName "Time" hdr
      first = head vals
      home = (readGroundPosition WGS84 $ pf first)
      (_, vals') = L.mapAccumL (\a b -> let c = readGroundPosition WGS84 $ pf b
                                            d = distance home c
                                            c' = readGroundPosition WGS84 $ pf a
                                            t = parseTS (df b) (tf b) tz
                                            t' = parseTS (df a) (tf a) tz
                                            s = speed t' t c c' in
                                          (b, b V.++ V.fromList [show (d D./~ meter), show s]))
                   first (tail vals) in
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
