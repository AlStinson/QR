module Codec.QR.String.Encode where

import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version
import Codec.QR.String.Mode
import Codec.QR.String.Mode.Terminator

import Data.BitString
import Data.List (minimumBy)
import Data.Function (on)
import Data.Array

type ModeSelecting = (DataSet,Int)
type Segment = (DataSet,String)

encodeString :: Version -> [Segment] -> BitString
encodeString v = foldr f $ terminator v
   where f (x,y) bs = modeIndicator x v ++ integralToBitString 
                       (characterCountLength x v) (length y) ++ 
                        toBitString x y ++ bs

costsPerVersion :: String -> Version -> [([Segment],Int)]
costsPerVersion s minV = drop (index (MV 1, V 40) minV) $
                    [selectModes s $ MV n | n<-[1..4]] ++
                     replicate 9  (selectModes s $ V 1 ) ++
                     replicate 17 (selectModes s $ V 10) ++
                     replicate 14 (selectModes s $ V 27)

selectModes :: String -> Version -> ([Segment],Int)
selectModes s v = (recoverString s $ reverse modes,cost)
   where (modes,cost) = minCostModes v $ map exclusiveSet s
     
minCostModes :: Version -> [DataSet] -> ([ModeSelecting],Int)
minCostModes v s =  minimumBy f $ foldl go [([],0)] s
   where go xs c = [minimumBy f [include v a b | b<-xs] | a<-posibleSet c v]
         f = compare `on` snd

recoverString :: String -> [ModeSelecting] -> [(DataSet,String)]
recoverString [] [] = []
recoverString xs ((d,n):ys) = (d,a):(recoverString b ys)
   where (a,b) = splitAt n xs 

include :: Version -> DataSet -> ([ModeSelecting],Int) -> ([ModeSelecting],Int)
include v c ([],_) = ([(c,1)], charCost c 1 + headerCost v c)
include v c (ys@((d,m):xs),n) 
   | d == c = ((d,m+1):xs, n + charCost c (m+1))
   | otherwise = ((c,1):ys, n + charCost c 1 + headerCost v c)

capacityMinVersion :: ECLevel -> String -> Version
capacityMinVersion e s = max (modeMinVersion $ maximum $ map exclusiveSet s) 
                         $ go 0 (length s) (range (MV 1, V 40))
   where table = capacityMinVersionTable
         go n l (v:vs) = if l <= table ! (e,v) then v else go (n+1) l vs
         go _ _ [] = error "QR Codes cant hold that amount of data"

capacityMinVersionTable :: Array (ECLevel,Version) Int
capacityMinVersionTable = listArray ((L, MV 1),(H,V 40))
     [5,10,23,35,41,77,127,187,255,322,370,461,552,652,772,883,1022,1101,1250,1408,1548,1725,1903,2061,2232,2409,2620,2812,3057,3283,3517,3669,3909,4158,4417,4686,4965,5253,5529,5836,6153,6479,6743,7089,
      u,8,18,30,34,63,101,149,202,255,293,365,432,513,604,691,796,871,991,1082,1212,1346,1500,1600,1708,1872,2059,2188,2395,2544,2701,2857,3035,3289,3486,3693,3909,4134,4343,4588,4775,5039,5313,5596,
      u,u,u,21,27,48,77,111,144,178,207,259,312,364,427,489,580,621,703,775,876,948,1063,1159,1224,1358,1468,1588,1718,1804,1933,2085,2181,2358,2473,2670,2805,2949,3081,3244,3417,3599,3791,3993,
      u,u,u,u,17,34,58,82,106,139,154,202,235,288,331,374,427,468,530,602,674,746,813,919,969,1056,1108,1228,1286,1425,1501,1581,1677,1782,1897,2022,2157,2301,2361,2524,2625,2735,2927,3057]
   where u = 0
