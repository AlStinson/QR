module Codec.QR.Mode.Terminator where

import Codec.QR.Version
import Data.BitString

terminator :: Int -> Version -> BitString
terminator n = integralToBitString 0 . terminatorLength n
-- no puedo quitar la n ??? 

terminatorLength :: Int -> Version -> Int
terminatorLength n = numberVersionCase f g
   where f v = min (2*v+1) n
         g _ = min 4 n