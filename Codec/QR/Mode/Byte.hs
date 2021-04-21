module Codec.QR.Mode.Byte 
   (
    is,
    modeIndicator,
    characterCountLength,
    toBitString,
    minVersion
   ) where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level

import Data.Char
import Data.BitString

is :: Char -> Bool
is c = ord c < 256

minVersion :: ECLevel -> Version
minVersion = MV 3

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g
   where f 1 = error "Byte mode not available in MV-1"
         f 2 = error "Byte mode not available in MV-2"
         f n = integralToBitString 2 (n-1) 
         g _ = integralToBitString 4 4
        

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f 1 = error "Byte mode not available in MV-1"
         f 2 = error "Byte mode not available in MV-2"
         f n = n+1
         g n | n<= 9 =  8
             | n<=40 = 16

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:xs) = integralToBitString (ord x) 8 ++
                         toBitString xs 