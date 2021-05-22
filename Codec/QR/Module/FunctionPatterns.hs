module Codec.QR.Module.FunctionPatterns (blankQR) where

import Codec.QR.Module
import Codec.QR.Version

placeModules :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int
             -> [(Module,Bool)]
placeModules f n m i j = [((a+i,b+j),f a b) | a<-[0..n-1], b<-[0..m-1]]

finderPattern :: Int -> Int -> [(Module,Bool)]
finderPattern = placeModules f 7 7 
   where f x y = or [x==0, x==6, y==0, y==6, 
                     x>=2 && x<=4 && y>=2 && y<=4]

alignmentPatter :: Int -> Int -> [(Module,Bool)]
alignmentPatter = placeModules f 5 5 
   where f x y = or [x==0, x==4, y==0, y==4, x==2 && y==2]



vWhite, hWhite :: Int -> Int -> [(Module,Bool)]
vWhite = placeModules f 7 1
   where f _ _ = False
hWhite = placeModules f 1 8 
   where f _ _ = False

blackAndWhite :: Int -> Int -> Int -> Int -> [(Module,Bool)]
blackAndWhite = placeModules f 
   where f x y = mod (x+y) 2 == 0

reservedArea :: Int -> Int -> Int -> Int -> [(Module,Bool)]
reservedArea = placeModules f
   where f _ _ = False


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