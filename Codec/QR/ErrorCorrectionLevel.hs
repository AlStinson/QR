module Codec.QR.ErrorCorrectionLevel where

import Codec.QR.Version

import Data.BitString
import Data.Bool.Instances

import Data.Ix

data ECLevel = L | M | Q | H
   deriving (Eq, Ord, Enum, Show, Ix)

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

ecLevelMinVersion :: ECLevel -> Version
ecLevelMinVersion ecl = case ecl of
   L -> MV 1
   M -> MV 2
   Q -> MV 4
   H -> V  1

maxECLevel :: Version -> ECLevel
maxECLevel = numberVersionCase (\x -> [L,M,M,Q] !! (x-1)) (const H)