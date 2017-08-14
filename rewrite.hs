import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

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

processRow :: (V.Vector String) -> Coord -> Float -> [V.Vector String] -> IO Float
processRow _ _ x [] = return x
processRow hdr home largest (x:xs) =
  let coords = toCoordS $ x V.! 11
      d = distance home coords
      m = max largest d in
    do
      putStrLn $ show x
      putStrLn $ show home ++ " -> " ++ show coords ++ " = " ++ show d
      processRow hdr home m xs


process hdr vals =
  processRow hdr (toCoordS $ head vals V.! 11) 0 vals

main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
        Left err -> putStrLn err
        Right v ->
          do
            md <- process (V.head v) $ take 100 $ V.toList $ V.tail v
            putStrLn $ show md
