#!/usr/bin/env stack
-- stack --system-ghc runghc --package cassava --package geodetics

import Data.Csv
import Geodetics.Geodetic
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V

distance (Just a) (Just b) = case groundDistance a b of
                               Nothing -> _0
                               Just (d, _, _) -> if (isNaN (d /~ meter)) then _0 else d

byName :: String -> (V.Vector String) -> (V.Vector String) -> String
byName field hdr = case V.elemIndex field hdr of
                     Nothing -> (\_ -> "")
                     Just n -> (\r -> r V.! n)

process :: (V.Vector String) -> [V.Vector String] -> [V.Vector String]
process hdr vals =
  let pf = byName "GPS" hdr
      home = (readGroundPosition WGS84 $ pf (head vals)) in
    (V.snoc hdr "distance") : L.map (\b ->
                                       let c = readGroundPosition WGS84 $ pf b
                                           d = distance home c in
                                         V.snoc b (show (d /~ meter))) vals

main :: IO ()
main = do
    file <- fmap head getArgs
    csvData <- BL.readFile file
    case decode NoHeader csvData :: Either String (V.Vector (V.Vector String)) of
        Left err -> putStrLn err
        Right v ->
            BL.putStr $ encode $ process (V.head v) $ V.toList $ V.tail v
