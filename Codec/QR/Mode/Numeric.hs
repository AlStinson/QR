module Codec.QR.Mode.Numeric 
   (
    is,
    modeIndicator,
    characterCountLength,
    toBitString,
    minVersion,
    charCost
   ) where

import Codec.QR.Core
import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level 

is :: Char -> Bool
is c = elem c ['0'..'9']

minVersion :: ECLevel -> Version
minVersion = MV 1

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g 
   where f n = integralToBitString (n-1) 0
         g _ = integralToBitString 4 1

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f n = n+2
         g n | n<=  9 = 10
             | n<= 26 = 12
             | n<= 40 = 14

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:y:z:xs) = (integralToBitString 10 (read [x,y,z] :: Int))
                         ++ toBitString xs  
toBitString (x:y:_) = integralToBitString 7 (read [x,y] :: Int) 
toBitString (x:_) = integralToBitString 4 (read [x] :: Int) 

charCost :: Int -> Int
charCost d = case mod d 3 of
   0 -> 3
   1 -> 4
   2 -> 3