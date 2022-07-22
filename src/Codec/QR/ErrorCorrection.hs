module Codec.QR.ErrorCorrection where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Module.Count
import Codec.ReedSolomon
import Data.GF256
import Data.Array

rsCodes :: ECLevel -> Version -> [RSCode]
rsCodes e v = [rsCode (d + if n>b-r then 1 else 0) f m  | n<-[1..b]]
   where (d,r) = divMod (dataCwCount e v) b
         f = ecCwPerBlock ! (e,v)
         m = miscodeProtectionCw e v
         b = ecBlocks ! (e,v)

wordsLength :: ECLevel -> Version -> [Int]
wordsLength e = shortLastwordVersionCase f (const c)
   where f v = take (dataCwCount e v - 1) c ++ (4:c) ++ repeat 8
         c = repeat 8


padCodewords :: Int -> Version -> [GF256]
padCodewords n =  shortLastwordVersionCase
                   (const $ if n==0 then [] else (take (n-1) c) ++ [0])
                   (const $ take n c) 
   where c = cycle [236,17]


ecBlock :: (RSCode,[GF256]) -> [GF256]
ecBlock (r,s) = ecCodewords r s


miscodeProtectionCw :: ECLevel -> Version -> Int
miscodeProtectionCw e v | number v > 4 = 0
                        | otherwise = miscodeProtectionCw4 ! (e,v) 

miscodeProtectionCw4 :: Array (ECLevel,Version) Int
miscodeProtectionCw4 = listArray ((L,MV 1),(H,V 4))
-- MV 1, 2, 3, 4, V 1, 2, 3, 4  EC
  [   2, 3, 2, 2,   3, 2, 1, 0, -- L
      u, 2, 2, 0,   2, 0, 0, 0, -- M
      u, u, u, 0,   1, 0, 0, 0, -- Q
      u, u, u, u,   1, 0, 0, 0  -- H
  ] where u = undefined

ecBlocks :: Array (ECLevel, Version) Int 
ecBlocks = listArray ((L, MV 1),(H,V 40))
-- MV 1, 2, 3, 4, V 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40  EC 
  [   1, 1, 1, 1,   1, 1, 1, 1, 1, 2, 2, 2, 2,  4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25, -- L
      u, 1, 1, 1,   1, 1, 1, 2, 2, 4, 4, 4, 5,  5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49, -- M
      u, u, u, 1,   1, 1, 2, 2, 4, 4, 6, 6, 8,  8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68, -- Q
      u, u, u, u,   1, 1, 2, 4, 4, 4, 5, 6, 8,  8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81  -- H
  ] where u = undefined

ecCwPerBlock :: Array (ECLevel,Version) Int
ecCwPerBlock = listArray ((L, MV 1),(H,V 40))
-- MV 1, 2, 3,  4, V  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40  EC
  [   2, 5, 6,  8,    7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, -- L
      u, 6, 8, 10,   10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, -- M
      u, u, u, 14,   13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, -- Q
      u, u, u,  u,   17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30  -- H
  ] where u = undefined