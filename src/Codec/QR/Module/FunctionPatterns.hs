module Codec.QR.Module.FunctionPatterns (blankQR) where

import Codec.QR.Module
import Codec.QR.Version

placeModules :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int ->
                [(Module,Bool)]
placeModules f n m i j = [((a+i,b+j),f a b) | a<-[0..n-1], b<-[0..m-1]]

finderPattern :: Int -> Int -> [(Module,Bool)]
finderPattern = placeModules f 7 7 
   where f x y = or [x==0, x==6, y==0, y==6, x>=2 && x<=4 && y>=2 && y<=4]

alignmentPatter :: Int -> Int -> [(Module,Bool)]
alignmentPatter = placeModules f 5 5 
   where f x y = or [x==0, x==4, y==0, y==4, x==2 && y==2]

vWhite, hWhite :: Int -> Int -> [(Module,Bool)]
vWhite = placeModules (const $ const $ False) 7 1
hWhite = placeModules (const $ const $ False) 1 8 

blackAndWhite :: Int -> Int -> Int -> Int -> [(Module,Bool)]
blackAndWhite = placeModules $ \x y -> mod (x+y) 2 == 0

reservedArea :: Int -> Int -> Int -> Int -> [(Module,Bool)]
reservedArea = placeModules $ const $ const $ False


blankQR :: Version -> [(Module,Bool)]
blankQR = numberVersionCase blankQRMV blankQRV

blankQRV :: Int -> [(Module,Bool)]
blankQRV v = let s = sizeV v in concat $ 
   [finderPattern 0 0, finderPattern 0 (s-6), finderPattern (s-6) 0,
    vWhite 0 7, vWhite 0 (s-7), vWhite (s-6) 7,
    hWhite 7 0, hWhite 7 (s-7), hWhite (s-7) 0,
    blackAndWhite 1 (s-15) 6 8, blackAndWhite (s-15) 1 8 6,
    blackAndWhite 1 1 (s-7) 8, reservedArea 6 1 0 8, 
    reservedArea 2 1 7 8, reservedArea 1 1 8 7, reservedArea 1 6 8 0,
    reservedArea 1 8 8 (s-7), reservedArea 7 1 (s-6) 8,
    if v >= 7 then reservedArea 6 3 0 (s-10) else [],
    if v >= 7 then reservedArea 3 6 (s-10) 0 else []] ++
    [alignmentPatter (i-2) (j-2) | (i,j)<- alignmentPatternLocationV v] 

blankQRMV :: Int -> [(Module,Bool)]
blankQRMV v = let s = sizeMV v in concat $ 
   [finderPattern 0 0, vWhite 0 7, hWhite 7 0, 
    blackAndWhite 1 (s-7) 0 8, blackAndWhite (s-7) 1 8 0,
    vWhite 1 8, hWhite 8 1]