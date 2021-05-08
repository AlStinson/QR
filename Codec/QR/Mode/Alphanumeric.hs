module Codec.QR.Mode.Alphanumeric 
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
is c = elem c set

minVersion :: ECLevel -> Version
minVersion = MV 2

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g
   where f 1 = error "Alphanumeric not available in MV-1"
         f n = integralToBitString (n-1) 1
         g _ = integralToBitString 4 2

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f 1 = error "Alphanumeric mode not available in MV-1"
         f n = n+1
         g n | n<= 9 = 9
             | n<=26 = 11
             | n<=40 = 13

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:y:xs) = integralToBitString 11 ((toInt x)*45+(toInt y)) ++ 
                       toBitString xs
toBitString (x:_) = integralToBitString 6 $ toInt x

set = ['0'..'9']++['A'..'Z']++" $%*+-./:"

charCost :: Int -> Int
charCost d = case mod d 2 of
   0 -> 5
   1 -> 6

toInt :: Char -> Int
toInt = go set 0
   where go (x:xs) n a | x==a = n
                       | otherwise = go xs (n+1) a  

fromInt :: Int -> Char
fromInt n = set !! n