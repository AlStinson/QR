module Codec.QR.String.Mode.Numeric
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
import Data.Char (ord)

is :: Char -> Bool
is c = let o = ord c in o>=48 && o<=57

minVersion :: Version
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

fromBitString :: BitString -> Maybe String
fromBitString xs = go xs (length xs) [] 
   where go :: BitString -> Int -> String -> Maybe String
         go xs n ys | n==0  = Just ys
                    | n==4  = Just $ (ys++) $ show $ bitStringToNum xs
                    | n==7  = Just $ (ys++) $ showInt 2 $ bitStringToNum xs
                    | n>=10 = let (a,b) = splitAt 10 xs
                              in go b (n-10) $ (ys++) $ showInt 3 $ 
                                 bitStringToNum a
                    | otherwise = Nothing

charCost :: Int -> Int
charCost d = case mod d 3 of
   0 -> 3
   1 -> 4
   2 -> 3


showInt :: Int -> Int -> String
showInt k n = let s = show n in replicate (k-(length s)) '0' ++ s 
