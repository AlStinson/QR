module Codec.QR where

import Codec.QR.String.Optimization
import Codec.QR.String.Mode
import Codec.QR.String.Encode
import Codec.QR.String.Decode
import Codec.QR.ErrorCorrection
import Codec.QR.ErrorCorrection.Encode
import Codec.QR.ErrorCorrection.Decode
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Score
import Codec.QR.Information.Format
import Codec.QR.Information.Version
import Codec.QR.Module
import Codec.QR.Module.Count
import Codec.QR.Module.FunctionPatterns
import Codec.QR.QR
import Codec.QR.Mask
import Codec.QR.Image
import Codec.QR.Version


import Data.BitString
import Data.GF256
import Data.Bits (xor)

prueba s = decodeQR . makeQR s

decodeQR :: QR -> Maybe String
decodeQR qr = do 
   v <- getVersion qr
   (e,m) <- getFormatInformation qr v
   bitString <- decodeData e v $ take 
               (nonReservedModCount v - remainderBitsCount v) $ 
               [ xor (qr ! p) $ mask m v p | p<-nonReservedMod v]
   decodeString v $ bitString
 

makeQR :: String -> ECLevel -> QR
makeQR s e = makeQRWith (Left s) e Nothing Nothing

makeQRWith :: Either String [(DataSet,String)] -> 
              ECLevel -> 
              Maybe Version -> 
              Maybe Mask -> 
--              Maybe Encoding -> 
              QR 
makeQRWith a e b c =  (qr //) $ 
                    (zip (versionLocation v) $ (vi++vi)) ++
                    (zip (formatLocation v) $ (fi++fi))
   where minV = maximum [ecLevelMinVersion e, maybe (MV 1) id b, 
                         minVersion $ maximum $ either 
                          (map exclusiveSet) (map fst) a]
         ls = drop (index (MV 1, V 40) minV) $ costs $ 
               either selectModes preEncodeString a
         v = findVersion e minV $ map fst ls
         pre = snd $ ls !! (index (minV, V 40) v)
         unMaskQR = array ((0,0),(size v,size v)) $ (blankQR v ++) $
                    zip (nonReservedMod v) $ (++remainderBits v) $ 
                     encodeData e v $ encodeString v pre 
         mask = maybe (betterScore unMaskQR v) id c
         qr = applyMask mask v unMaskQR
         fi = encodeFormat e v mask
         vi = encodeVersion v




findVersion :: ECLevel -> Version -> [Int] -> Version
findVersion e m = go $ range (m,V 40) 
   where go [] [] = error "QR codes cant hold that amount of data"
         go (v:vs) (c:cs) | c <= dataModCount e v = v
                          | otherwise = go vs cs


ecLevelMinVersion :: ECLevel -> Version
ecLevelMinVersion ecl = case ecl of
   L -> MV 1
   M -> MV 2
   Q -> MV 4
   H -> V  1