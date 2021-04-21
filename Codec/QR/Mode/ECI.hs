module Codec.QR.Mode.ECI where

import Data.BitString

type ECI = Int -- ECI INDICATOR

eciDesignatorLength :: ECI -> Int
eciDesignatorLength n | n < 127   = 7
                      | n < 16383 = 14
                      | otherwise = 21

eciDesignator :: ECI -> BitString
eciDesignator n = 
   where l = eciDesignatorLegnth n
         


data ECI = ECI BitString -- ECI Indicator