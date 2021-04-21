module Codec.QR.Format where

import Codec.QR.Core
import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level

import Data.Poly

decodeFormatV :: BitString -> (ECLevel, Mask) 
decodeFormatV s = undefined
decodeFormatMV = undefined


encodeFormat :: Version -> Mask -> BitString
encodeFormat v m = zipWith (+) (infoMask v) $ encodeWithoutMask $ 
                   versionCase vIndicatorMV (ecLevelIndicator . ecl) v ++ 
                   integralToBitString m (kindVersionCase 2 3 v)

encodeWithoutMask :: BitString -> BitString
encodeWithoutMask s = (s ++) $ drop 1 $ coefs $ 
                      p + polyMod (p*(makePoly s)) polyGen
   where p = monomial 1 10 

infoMask :: Version -> BitString
infoMask = kindVersionCase p q
   where p = [1,0,0,0,1,0,0,0,1,0,0,0,1,0,1]
         q = [1,0,1,0,1,0,0,0,0,0,1,0,0,1,0]

polyGen :: Poly Bool
polyGen = makePoly [1,0,1,0,0,1,1,0,1,1,1]

vIndicatorMV :: Version -> BitString
vIndicatorMV v = case v of
   (MV 1 L) -> [0,0,0]
   (MV 2 L) -> [0,0,1]
   (MV 2 M) -> [0,1,0]
   (MV 3 L) -> [0,1,1]
   (MV 3 M) -> [1,0,0]
   (MV 4 L) -> [1,0,1]
   (MV 4 M) -> [1,1,0]
   (MV 4 Q) -> [1,1,1]