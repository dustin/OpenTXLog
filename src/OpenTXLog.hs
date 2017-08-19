module OpenTXLog (
  FieldLookup
                 ) where

import qualified Data.Vector as V

type FieldLookup = String -> (V.Vector String) -> String
