module Codec.QR.Format where

import Codec.QR.Version
import Codec.QR.ErrorCorrectionLevel
import Codec.QR.Mask
import Codec.QR.Module
import Codec.QR.QR

import Data.BitString
import Data.Poly


getFormatInformation :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformation qr = versionCase 
                          (getFormatInformationMV qr)
                          (getFormatInformationV qr)

getFormatInformationMV :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformationMV qr v = decodeFormat 
                             [qr ! p | p<-formatLocation v] v

getFormatInformationV :: QR -> Version -> Maybe (ECLevel,Mask)
getFormatInformationV qr v = go (decodeFormat f1 v) (decodeFormat f2 v)
   where (f1,f2) = splitAt 15 [qr ! p | p<-formatLocation v]
         go Nothing Nothing = Nothing
         go Nothing x       = x
         go x       Nothing = x
         go x       y       | x==y      = x
                            | otherwise = Nothing 


decodeFormat :: BitString -> Version -> Maybe (ECLevel, Mask) 
decodeFormat s v = go [(encodeFormat e v m,(e,m)) | 
                         e<-[L .. maxECLevel v], m<-[0..maxMask v]]
   where count = foldl (\x y -> x + if y then 1 else 0) 0
         go [] = Nothing
         go ((x,y):xs) | (3>=) $ count $ zipWith (+) s x  = Just y
                       | otherwise = go xs
         
encodeFormat :: ECLevel -> Version -> Mask -> BitString
encodeFormat e v m = zipWith (+) (infoMask v) $ encodeWithoutMask $ 
                   versionCase (vIndicatorMV e) (const $ ecLevelIndicator e) v
                   ++ integralToBitString (kindVersionCase 2 3 v) m

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