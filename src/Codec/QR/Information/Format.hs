module Codec.QR.Information.Format where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Mask
import Codec.QR.Module
import Codec.QR.QR
import Codec.BHC
import Data.BitString


getFormatInformation :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformation qr = versionCase 
                          (getFormatInformationMV qr)
                          (getFormatInformationV qr)

getFormatInformationMV :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformationMV qr v = decodeFormat (formatModules qr v) v

getFormatInformationV :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformationV qr v = go (decodeFormat f1 v) (decodeFormat f2 v)
   where (f1,f2) = splitAt 15 $ formatModules qr v
         go Nothing y       = y
         go x       Nothing = x
         go x       y       = if x==y then x else Nothing

formatModules :: QR -> Version -> BitString
formatModules qr v = [qr ! p | p<-formatLocation v]

decodeFormat :: BitString -> Version -> Maybe (ECLevel, Mask) 
decodeFormat s v = bhcDecode bhcCodeFormat s [(encodeFormat e v m,(e,m)) | 
                   e<-[L .. maxECLevel v], m<-[0..maxMask v]]

         
encodeFormat :: ECLevel -> Version -> Mask -> BitString
encodeFormat e v m = zipWith (+) (infoMask v) $ bhcEncode bhcCodeFormat $ 
                   versionCase (vIndicatorMV e) (const $ ecLevelIndicator e) v
                   ++ integralToBitString (kindVersionCase 2 3 v) m


bhcCodeFormat :: BHCCode
bhcCodeFormat = BHC 15 5 3

infoMask :: Version -> BitString
infoMask = kindVersionCase p q
   where p = [1,0,0,0,1,0,0,0,1,0,0,0,1,0,1]
         q = [1,0,1,0,1,0,0,0,0,0,1,0,0,1,0]

ecLevelIndicator :: ECLevel -> BitString
ecLevelIndicator ecl = case ecl of
                    L -> [0,1]
                    M -> [0,0]
                    Q -> [1,1]
                    H -> [1,0]

indicatorECLevel :: BitString -> ECLevel
indicatorECLevel s = case s of
                  [0,0] -> M
                  [0,1] -> L
                  [1,0] -> H
                  [1,1] -> Q
                  _     -> error $ show s++" is not a indicator"

vIndicatorMV :: ECLevel -> Version -> BitString
vIndicatorMV e v = case (e,v) of
   (L, MV 1) -> [0,0,0]
   (L, MV 2) -> [0,0,1]
   (M, MV 2) -> [0,1,0]
   (L, MV 3) -> [0,1,1]
   (M, MV 3) -> [1,0,0]
   (L, MV 4) -> [1,0,1]
   (M, MV 4) -> [1,1,0]
   (Q, MV 4) -> [1,1,1]

maxECLevel :: Version -> ECLevel
maxECLevel = numberVersionCase (\x -> [L,M,M,Q] !! (x-1)) (const H)