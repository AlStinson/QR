module Codec.QR where

import Codec.QR.Core

import Codec.QR.Mode
import Data.GF256
import Codec.QR.ErrorCorrection
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Score
import Codec.QR.Version
import Codec.QR.Version.Information
import Codec.QR.Module
import Codec.QR.Format


import Codec.QR.Mask
-----

showQR :: QR -> IO ()
showQR t = putStrLn $ nlines $ spaces $ g 0 0
   where g n m | m == s && n== s = c:c:nlines []
               | m == s          = c:c:'\n':(spaces $ g (n+1) 0)
               | otherwise       = c:c:(g n (m+1))
            where c = if t!(n,m) then '█' else ' ' 
         s = snd $ snd $ bounds t
         spaces s = ' ':' ':' ':' ':' ':' ':' ':' ':s
         nlines s = '\n':'\n':'\n':'\n':s

----

makeQR :: String -> ECLevel -> QR
makeQR s e = makeQRWith (Left s) (Left e) Nothing

makeQRWith :: Either String [(DataSet,String)] -> 
              Either ECLevel Version -> 
              Maybe Mask -> 
              QR
makeQRWith a b c = update qr $ 
                    (zip (versionLocation v) (vi++vi)) ++
                    (zip (formatLocation v) (fi++fi))
   where ds = case a of 
               Left x -> dataAnalisis x
               Right x -> x
         pre = preEncodeString ds
         v = case b of 
                 Left x -> findVersion pre x
                 Right x -> x
         l = bitLength v pre
         r = dataModCount v - l
         s = size v
         unMaskQR = listTable s s $ (blankQR v ++) $
                    putData v $ makeBitString pre v l r ++ remainderBits v
         (qr,m) = case c of
                   Nothing -> betterScore v unMaskQR
                   Just x  -> (applyMask x v unMaskQR, x)
         fi = encodeFormat v m
         vi = encodeVersion v



makeBitString :: [(DataSet,BitString,Int,Int)] -> Version -> Int -> Int -> BitString
makeBitString pre v l r = concatMap 
 (\x -> integralToBitString (unpack x) 8) $ encodeData v $ 
 (bitStringToGF256 (l+t) (r-t) v $ encodeString v r pre) ++ 
 (padCodewords (r-t) v )
   where t = terminatorLength r v


bitStringToGF256 :: Int -> Int -> Version -> BitString -> [GF256]
bitStringToGF256 l r v bs = numberVersionCase 
                        (bitStringToGF256MV l r bs) 
                        (const $ bitStringToGF256V l bs) v

bitStringToGF256V :: Int -> BitString -> [GF256]
bitStringToGF256V n bs | n>=8 = (bitStringToNum b):
                                 (bitStringToGF256V (n-8) e)
                       | n==0 = []
                       | otherwise = [bitStringToNum $ 
                                      bs ++ (take (8-n) $ repeat False)]
   where (b,e) = splitAt 8 bs

bitStringToGF256MV :: Int -> Int -> BitString -> Int -> [GF256]
bitStringToGF256MV l r bs v | even v = bitStringToGF256V l bs
                            | l >= 8 = (bitStringToNum b):
                                     (bitStringToGF256MV (l-8) r e v)
                            | r > 4 = [bitStringToNum $
                                      bs ++ (take (8-l) $ repeat False)]
                            | r == 4 = [bitStringToNum bs]
                            | r < 4 = [bitStringToNum $ 
                                      bs ++ (take (4-l) $ repeat False)]
   where (b,e) = splitAt 8 bs
         c = dataModCount (MV v L) -- Da igual el ECL

encodeString :: Version -> Int -> [(DataSet,BitString,Int,Int)] -> BitString
encodeString v n = foldr f (terminator n v) 
   where f (x,y,z,w) bs = modeIndicator x v ++ (integralToBitString w 
                        (characterCountLength x v)) ++ y ++ bs


bitLength :: Version -> [(DataSet, BitString, Int,Int)] -> Int
bitLength v = foldl f 0
   where f n (a,_,b,_) = modeIndicatorLength v + 
                       characterCountLength a v + 
                       b + n


findVersion :: [(DataSet,BitString, Int,Int)] -> ECLevel -> Version
findVersion ds e = go $ range (m, V 40 e) 
   where m = maximum $ (ecLevelMinVersion e):
              (map (\(x,_,_,_)-> minVersion x e) ds)
         go [] = error "QR codes cant hold that amount of data"
         go (v:vs) | l <= c = v
                   | otherwise = go vs
            where l = bitLength v ds
                  c = dataModCount v


padCodewords :: Int -> Version -> [GF256]
padCodewords n =  numberVersionCase f g
   where c = div n 8
         c2 = div (n-4) 8
         f v | even v = take c padCodewordsV
             | odd v  = take c2 padCodewordsV ++
                        if n>=4 then [0] else []
         g _ = take c padCodewordsV

padCodewordsV :: [GF256]
padCodewordsV = 236:17:padCodewordsV


putData :: Version -> BitString -> [(Module,Bool)]
putData v = zip (nonReservedMod v)


blankQR :: Version -> [(Module,Bool)]
blankQR = numberVersionCase blankQRMV blankQRV

blankQRV :: Int -> [(Module,Bool)]
blankQRV v = concat $ 
   [finderPattern 0 0, finderPattern 0 (s-7), finderPattern (s-7) 0,
    vWhite 0 7, vWhite 0 (s-8), vWhite (s-7) 7,
    hWhite 7 0, hWhite 7 (s-8), hWhite (s-8) 0,
    blackAndWhite 1 (s-16) 6 8, blackAndWhite (s-16) 1 8 6,
    blackAndWhite 1 1 (s-8) 8, reservedArea 6 1 0 8, 
    reservedArea 2 1 7 8, reservedArea 1 1 8 7, reservedArea 1 6 8 0,
    reservedArea 1 8 8 (s-8), reservedArea 7 1 (s-7) 8,
    if s>=45 then reservedArea 6 3 0 (s-11) else [],
    if s>=45 then reservedArea 3 6 (s-11) 0 else []] ++
    [alignmentPatter (i-2) (j-2) | (i,j)<- alignmentPatternLocationV v] 
   where s = sizeV v

blankQRMV :: Int -> [(Module,Bool)]
blankQRMV v = concat $ 
   [finderPattern 0 0, vWhite 0 7, hWhite 7 0, 
    blackAndWhite 1 (s-8) 0 8, blackAndWhite (s-8) 1 8 0,
    vWhite 1 8, hWhite 8 1]
   where s = sizeMV v