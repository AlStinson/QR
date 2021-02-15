module Codec.QR.Mode.Byte where

import Data.Char
import Data.BitString

byteToBitString :: String -> BitString
byteToBitString [] = []
byteToBitString (x:xs) = integralToBitString (ord x) 8 ++
                         byteToBitString xs 