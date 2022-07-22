module Codec.QR.String.Mode.Alphanumeric
  ( is
  , minVersion 
  , modeIndicator
  , characterCountLength 
  , toBitString
  , fromBitString
  , charCost
  ) where 

import Codec.QR.Version
import Data.BitString

is :: Char -> Bool
is c = elem c set

minVersion :: Version
minVersion = MV 2

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g
   where f 1 = error "Alphanumeric not available in version M1"
         f n = integralToBitString (n-1) 1
         g _ = integralToBitString 4 2

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f 1 = error "Alphanumeric mode not available in version M1"
         f n = n+1
         g n | n<= 9 = 9
             | n<=26 = 11
             | n<=40 = 13

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:y:xs) = integralToBitString 11 ((toInt x)*45+(toInt y)) ++ 
                       toBitString xs
toBitString (x:_) = integralToBitString 6 $ toInt x

fromBitString :: BitString -> Maybe String
fromBitString xs = go xs (length xs) []
   where go xs n ys | n==0  = Just $ reverse ys
                    | n==6  = go [] 0 $ (fromInt $ bitStringToNum xs):ys
                    | n>=11 = go b (n-11) $ (fromInt m):(fromInt d):ys
                    | otherwise = Nothing
            where (a,b) = splitAt 11 xs
                  (d,m) = divMod (bitStringToNum a) 45

charCost :: Int -> Int
charCost d = if even d then 5 else 6

set :: String
set = ['0'..'9']++['A'..'Z']++" $%*+-./:"

toInt :: Char -> Int
toInt = go set 0
   where go (x:xs) n a | x==a = n
                       | otherwise = go xs (n+1) a  

fromInt :: Int -> Char
fromInt n = set !! n