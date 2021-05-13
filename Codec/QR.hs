module Codec.QR where

import Codec.QR.Core

import Codec.QR.Mode
import Codec.QR.ErrorCorrection
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Score
import Codec.QR.Version
import Codec.QR.Version.Information
import Codec.QR.Module
import Codec.QR.Format
import Codec.QR.QR


import Codec.QR.Mask
import Codec.QR.Image

makeQR :: String -> ECLevel -> QR
makeQR s e = makeQRWith (Left s) (Left e) Nothing

makeQRWith :: Either String [(DataSet,String)] -> 
              Either ECLevel Version -> 
              Maybe Mask -> 
--              Maybe Encoding -> 
              QR 
makeQRWith a b c =  (qr //) $ 
                    (zip (versionLocation v) (vi++vi)) ++
                    (zip (formatLocation v) (fi++fi))
   where e = either id ecl b
         minV = maximum [ecLevelMinVersion e, either (MV 1) id b, 
                         flip minVersion e $ maximum $ 
                          either (map exclusiveSet) (map fst) a]
         ls = let g x v = let pre = preEncodeString x
                         in (bitLength v pre,pre)
              in drop (index (MV 1 e, V 40 e) minV) $ 
                 costs e $ either (flip selectModes) g a
         v = findVersion minV $ map fst ls
         s = size v
         pre = snd $ ls !! (index (minV, V 40 e) v)
         unMaskQR = array ((0,0),(s-1,s-1)) $ (blankQR v ++) $
                    zip (nonReservedMod v) $ makeBitString pre v ++ 
                                             remainderBits v
         (qr,m) = maybe (betterScore v unMaskQR) 
                        (\x -> (applyMask x v unMaskQR,x)) c
         fi = encodeFormat v m
         vi = encodeVersion v



makeBitString :: [InterSegment] -> Version -> BitString
makeBitString pre v = integralsToBitString (wordsLength v) $ encodeData v $ 
                      cw ++ (padCodewords (c - length cw) v)
   where m = dataModCount v
         c = dataCwCount v
         m' = 8*(div m 8)
         (a,b) = splitAt m' $ take m $ encodeString v pre
         cw = bitStringToNums 8 a ++ bitStringToNums 4 b
         en = encodeData v $ cw ++ (padCodewords (c - length cw) v)

findVersion :: Version -> [Int] -> Version
findVersion m = go (range (m, V 40 $ ecl m)) 
   where go [] [] = error "QR codes cant hold that amount of data"
         go (v:vs) (c:cs) | c <= dataModCount v = v
                          | otherwise = go vs cs

padCodewords :: Int -> Version -> [GF256]
padCodewords n =  shortLastwordVersionCase
                   (const $ if n==0 then [] else (take (n-1) c) ++ [0])
                   (const $ take n c) 
   where c = cycle [236,17]

wordsLength :: Version -> [Int]
wordsLength = shortLastwordVersionCase
               f (const c)
   where f v = take (dataCwCount v - 1) c ++ (4:c)
         c = repeat 8