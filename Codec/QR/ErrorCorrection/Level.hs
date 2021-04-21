module Codec.QR.ErrorCorrection.Level where

import Codec.QR.Core

data ECLevel = L | M | Q | H -- Error Correction Level
   deriving (Eq, Ord, Enum, Show)

ecLevelIndicator :: ECLevel -> BitString
ecLevelIndicator ecl = case ecl of
                    L -> [0,1]
                    M -> [0,0]
                    Q -> [1,1]
                    H -> [1,0]

indicatorECLevel :: BitString -> ECLevel
indicatorECLevel s = case s of
                  [0,0] -> M
                  [0,1] -> L
                  [1,0] -> H
                  [1,1] -> Q
                  _     -> error $ show s++" is not a indicator"
