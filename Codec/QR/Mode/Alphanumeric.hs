module Codec.QR.Mode.Alphanumeric (alphanumericToBitString)where

import Data.BitString
import Data.Char

alphanumericToBitString :: String -> BitString
alphanumericToBitString [] = []
alphanumericToBitString (x:y:xs) = integralToBitString  
                                   ((toInt x)*45+(toInt y)) 11 ++ 
                                   alphanumericToBitString xs
alphanumericToBitString (x:_) = integralToBitString (toInt x) 6

simbolos = ['0'..'9']++['A'..'Z']++" $%*+-./:"

toInt :: Char -> Int
toInt = go simbolos 0
   where go (x:xs) n a | x==a = n
                       | otherwise = go xs (n+1) a  

fromInt :: Int -> Char
fromInt n = simbolos !! n