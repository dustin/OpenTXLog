{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Csv
import qualified Data.Vector as V
import System.Environment
import GHC.Generics (Generic)

processRow :: (V.Vector ByteString) -> [V.Vector ByteString] -> IO ()
processRow hdr [] = return ()
processRow hdr (x:xs) =
  do
    putStrLn $ show x
    putStrLn $ show $ x V.! 11
    processRow hdr xs

process hdr vals = do
  putStrLn $ show hdr
  processRow hdr vals

main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decode NoHeader csvData :: Either String (V.Vector (V.Vector ByteString)) of
        Left err -> putStrLn err
        Right v -> process (V.head v) $ take 25 $ V.toList (V.tail v)
