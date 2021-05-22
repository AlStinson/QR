module Codec.QR where

import Codec.QR.Mode
import Codec.QR.ErrorCorrection
import Codec.QR.ErrorCorrectionLevel
import Codec.QR.Score
import Codec.QR.Version
import Codec.QR.Version.Information
import Codec.QR.Module
import Codec.QR.Module.Count
import Codec.QR.Module.FunctionPatterns
import Codec.QR.Format
import Codec.QR.QR
import Codec.QR.Mask
import Codec.QR.Image

import Data.BitString
import Data.GF256
import Data.Bits (xor)

prueba s = decodeQR . makeQR s

decodeQR :: QR -> Maybe String
decodeQR qr = do 
   v <- getVersion qr
   (e,m) <- getFormatInformation qr v
   cwString <- decodeData e v $ bitStringToNums (wordsLength e v) $  
               take (nonReservedModCount v - remainderBitsCount v) $ 
               [ xor (qr ! p) $ mask m v p | p<-nonReservedMod v]
   decodeString v $ integralsToBitString (wordsLength e v) cwString
 

makeQR :: String -> ECLevel -> QR
makeQR s e = makeQRWith (Left s) e Nothing Nothing

makeQRWith :: Either String [(DataSet,String)] -> 
              ECLevel -> 
              Maybe Version -> 
              Maybe Mask -> 
--              Maybe Encoding -> 
              QR 
makeQRWith a e b c =  (qr //) $ 
                    (zip (versionLocation v) (vi++vi)) ++
                    (zip (formatLocation v) (fi++fi))
   where minV = maximum [ecLevelMinVersion e, maybe (MV 1) id b, 
                         minVersion $ maximum $ either 
                          (map exclusiveSet) (map fst) a]
         ls = let g x v = let pre = preEncodeString x
                          in (bitLength v pre,pre)
              in drop (index (MV 1, V 40) minV) $ 
                 costs $ either (flip selectModes) g a
         v = findVersion e minV $ map fst ls
         s = size v
         pre = snd $ ls !! (index (minV, V 40) v)
         unMaskQR = array ((0,0),(s-1,s-1)) $ (blankQR v ++) $
                    zip (nonReservedMod v) $ makeBitString pre e v ++ 
                                             remainderBits v
         mask = maybe (betterScore unMaskQR v) id c
         qr = applyMask mask v unMaskQR
         fi = encodeFormat e v mask
         vi = encodeVersion v


makeBitString :: [InterSegment] -> ECLevel -> Version -> BitString
makeBitString pre e v = integralsToBitString (wordsLength e v) $
                        encodeData e v $ cw ++ (padCodewords (c - length cw) v)
   where cw = fst $ splitAt c $ bitStringToNums (wordsLength e v) $
              encodeString v pre
         c = dataCwCount e v

findVersion :: ECLevel -> Version -> [Int] -> Version
findVersion e m = go $ range (m,V 40) 
   where go [] [] = error "QR codes cant hold that amount of data"
         go (v:vs) (c:cs) | c <= dataModCount e v = v
                          | otherwise = go vs cs

padCodewords :: Int -> Version -> [GF256]
padCodewords n =  shortLastwordVersionCase
                   (const $ if n==0 then [] else (take (n-1) c) ++ [0])
                   (const $ take n c) 
   where c = cycle [236,17]

wordsLength :: ECLevel -> Version -> [Int]
wordsLength e = shortLastwordVersionCase
               f (const c)
   where f v = take (dataCwCount e v - 1) c ++ (4:c) ++ repeat 8
         c = repeat 8

maxECLevel :: Version -> ECLevel
maxECLevel = let f n = [L,M,M,Q] !! (n-1)
             in numberVersionCase f $ const H