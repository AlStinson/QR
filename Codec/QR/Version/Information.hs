module Codec.QR.Version.Information where

import Codec.QR.Version
import Codec.QR.Mask

import Data.Poly
import Data.BitString

encodeVersion :: Version -> BitString
encodeVersion v = (s ++) $ drop 1 $ coefs $ 
                  p + polyMod (p*(makePoly s)) polyGen
   where p = monomial 1 12 
         s = integralToBitString 6 (number v)

polyGen :: Poly Bool
polyGen = makePoly [1,1,1,1,1,0,0,1,0,0,1,0,1]
