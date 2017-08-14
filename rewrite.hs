{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Csv
import qualified Data.Vector as V
import System.Environment
import GHC.Generics (Generic)

data Coord = Coord { lat :: !Float , lon :: !Float } deriving (Show)

toCoord [lat, lon] = Coord lat lon

processRow :: (V.Vector String) -> [V.Vector String] -> IO ()
processRow hdr [] = return ()
processRow hdr (x:xs) =
  do
    let coords = toCoord $ map (\x -> read x :: Float) $ words (x V.! 11)
    putStrLn $ show x
    putStrLn $ show coords
    processRow hdr xs

process hdr vals = do
  putStrLn $ show hdr
  processRow hdr vals

main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
        Left err -> putStrLn err
        Right v -> process (V.head v) $ take 25 $ V.toList (V.tail v)
