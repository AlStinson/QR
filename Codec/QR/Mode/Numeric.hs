module Codec.QR.Mode.Numeric 
   (
    is,
    modeIndicator,
    characterCountLength,
    toBitString,
    minVersion
   ) where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level

import Data.BitString 

is :: Char -> Bool
is c = elem c ['0'..'9']

minVersion :: ECLevel -> Version
minVersion = MV 1

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g 
   where f n = integralToBitString 0 (n-1)
         g _ = integralToBitString 1 4

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f n = n+2
         g n | n<=  9 = 10
             | n<= 26 = 12
             | n<= 40 = 14

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:y:z:xs) = (integralToBitString (read [x,y,z] :: Int)  10)
                         ++ toBitString xs  
toBitString (x:y:_) = integralToBitString (read [x,y] :: Int)  7 
toBitString (x:_) = integralToBitString (read [x] :: Int)  4 