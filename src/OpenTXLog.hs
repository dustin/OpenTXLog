{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module OpenTXLog (
  FieldLookup
  , process
  , processCSVFile
  , parseRowTS
  , byName
  , dropDup
  -- transformations
  , RowTransformer
  , fieldTransformer
  , intFieldTransformer
  , r2dTransformer
  , Renamer
  , simpleRenamer
  , Transformer(..)
  ) where

import Data.Csv (HasHeader(..), decode)
import Data.Function (on)
import Data.Semigroup ((<>))
import Data.Time
import Text.Read (readMaybe)
import Data.Text (Text, unpack, pack, replace)
import Geodetics.Ellipsoids (Ellipsoid)
import Geodetics.Geodetic (Geodetic(..), WGS84(..), readGroundPosition, groundDistance)
import Numeric.Units.Dimensional.SIUnits
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Numeric.Units.Dimensional as D

type FieldLookup = Text -> V.Vector Text -> Text


-- hdr -> input row -> output row
type RowTransformer = V.Vector Text -> V.Vector Text -> V.Vector Text

-- hdr -> hdr
type Renamer = V.Vector Text -> V.Vector Text

data Transformer = Transformer ([V.Vector Text] -> RowTransformer) Renamer

fieldTransformer :: Text -> (Text -> Text) -> RowTransformer
fieldTransformer fname f hdr row =
  case V.elemIndex fname hdr of
    Nothing -> row
    (Just pos) -> V.update row (V.fromList [(pos, f (row V.! pos))])

intFieldTransformer :: Text -> (Int -> Int) -> RowTransformer
intFieldTransformer fname f =
  fieldTransformer fname (\it -> case (readMaybe . unpack) it of
                                   Nothing -> it
                                   (Just i) -> (pack.show.f) i)

r2dTransformer :: Text -> Transformer
r2dTransformer fname =
  let fname' = replace "(rad)" "(deg)" fname in
    Transformer
    (const $ fieldTransformer fname (\it -> case (readMaybe . unpack) it of
                                        Nothing -> it
                                        (Just (i::Double)) -> (pack.show) (i * (180/pi))))
    (simpleRenamer fname fname')

simpleRenamer :: Text -> Text -> Renamer
simpleRenamer f t = V.map (\h -> if h == f then t else h)

-- Parse a timestamp to a UTCTime given the timezone and a separate date and time string.
parseTS :: TimeZone -> Text -> Text -> UTCTime
parseTS tz ds ts =
  let l = defaultTimeLocale
      lt = (LocalTime <$> parseTimeM True l "%F" (unpack ds) <*> parseTimeM True l "%H:%M:%S%Q" (unpack ts)) in
    localTimeToUTC tz (lt())

-- Parse the timestamp out of a row.
parseRowTS :: TimeZone -> FieldLookup -> V.Vector Text -> UTCTime
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
byName :: V.Vector Text -> FieldLookup
byName hdr field = maybe (const "") (flip (V.!)) $ V.elemIndex field hdr

dropDup :: Eq a => (t -> a) -> [t] -> [t]
dropDup _ [] = []
dropDup pf (x:xs) = x:dropDup pf (dropWhile (on (==) pf x) xs)

minDur :: NominalDiffTime
minDur = 4

-- Remove entries from the head of a list that are within minDur of "now"
prune :: (V.Vector Text -> UTCTime)  -> [V.Vector Text] -> UTCTime -> [V.Vector Text]
prune pt vals now =
  let dt r = diffUTCTime now (pt r) in
    L.dropWhile ((> minDur) . dt) vals

-- Add distance and speed columns to telemetry logs.
process :: (V.Vector Text -> UTCTime) -> V.Vector Text -> [Transformer] -> [V.Vector Text] -> [V.Vector Text]
process pt hdr ts vals =
  let ts' = ts <> [Transformer hdist (`V.snoc` "distance"), Transformer noop (`V.snoc` "speed")]
      fs = map (\(Transformer f _) -> f vals) ts'
      vals' = map (\v -> foldr (\f o -> f hdr o) v fs) vals
      (_, vals'') = L.mapAccumL (\a r -> let c = rgp r
                                             t = pt r
                                             s = spd c t a in
                                           (prune pt a t, V.snoc r (d2s s)))
                   (dropDup pf vals') vals' in
    (foldl (\o (Transformer _ f) -> f o) hdr ts') : vals''

  where d2s = pack . printf "%.5f"
        spd _ _ [] = 0 -- no relevant movement
        spd c t a = let c' = rgp $ head a
                        t' = pt $ head a in speed t t' c c'
        pf = byName hdr "GPS"
        rgp = readGroundPosition WGS84 . unpack . pf
        noop = const . flip const

        -- Home distance transformer
        hdist :: [V.Vector Text] -> RowTransformer
        hdist vs = let home = foldr (\x o -> if pf x == "" then o else rgp x) Nothing vs in
                     \_ row ->
                       let c = rgp row
                           d = distance home c in
                         V.snoc row $ d2s (d D./~ meter)

processCSVFile :: String -> [Transformer] -> IO [V.Vector Text]
processCSVFile file ts = do
  tz <- getCurrentTimeZone
  csvData <- BL.readFile file
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector Text)) of
    Left err -> fail (show err)
    Right v ->
      let hdr = V.head v
          body = V.toList $ V.tail v in
        pure $ process (parseRowTS tz $ byName hdr) hdr ts body
