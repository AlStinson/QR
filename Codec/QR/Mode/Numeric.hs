module Codec.QR.Mode.Numeric where

import Data.BitString

numericToBitString :: String -> BitString
numericToBitString [] = []
numericToBitString (x:y:z:xs) = (integralToBitString (read [x,y,z] :: Int)  10) ++ numericToBitString xs  
numericToBitString (x:y:_) = integralToBitString (read [x,y] :: Int)  7 
numericToBitString (x:_) = integralToBitString (read [x] :: Int)  4 