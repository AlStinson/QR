module Codec.QR.Version.Information where

import Codec.QR.Core
import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level

import Data.Poly

decodeFormatV :: BitString -> (ECLevel, Mask) 
decodeFormatV s = undefined
decodeFormatMV = undefined


encodeVersion :: Version -> BitString
encodeVersion v = (s ++) $ drop 1 $ coefs $ 
                  p + polyMod (p*(makePoly s)) polyGen
   where p = monomial 1 12 
         s = integralToBitString (number v) 6

polyGen :: Poly Bool
polyGen = makePoly [1,1,1,1,1,0,0,1,0,0,1,0,1]
