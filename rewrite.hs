import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.List as L

data Coord = Coord { lat :: !Float , lon :: !Float } deriving (Show)

toCoord [lat, lon] = Coord lat lon

toCoordS s = toCoord $ map (\x -> read x :: Float) $ words s

d2r a = a * 0.0174532925

radlat (Coord l _) = d2r l
radlon (Coord _ l) = d2r l

distance p p2 =
  acos((sin(radlat p)*sin(radlat p2))+
        (cos(radlat p)*cos(radlat p2)*
          cos((radlon p)-(radlon p2)))) * 6371.01


process :: (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process hdr vals =
  let home = (toCoordS $ (head vals) V.! 11) in
    (V.snoc hdr "distance") : L.map (\b ->
                                       let c = toCoordS $ b V.! 11
                                           d = distance home c in
                                         V.snoc b (show d)) vals

main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
        Left err -> putStrLn err
        Right v ->
            BL.putStr $ encode $ process (V.head v) $ V.toList $ V.tail v
