module Codec.QR.ErrorCorrection where

import Data.List
import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Module
import qualified Codec.ReedSolomon as RS
import Data.GF256
import Data.Array

rsCodes :: Version -> [RS.RSCode]
rsCodes v = [RS.rsCode (d+ if n>b-r then 1 else 0) e m  | n<-[1..b]]
   where (d,r) = divMod (dataCwCount v) b
         e = ecCwPerBlock ! v 
         m = miscodeProtectionCw v
         b = ecBlocks ! v 

encodeData :: Version -> [GF256] -> [GF256]
encodeData v s = assemble (map snd blocks) (map ecBlock blocks)
   where blocks = blockDivision (rsCodes v) s
 
blockDivision :: [RS.RSCode] -> [GF256] -> [(RS.RSCode, [GF256])]
blockDivision [] [] = []
blockDivision (r:rs) xs = (r,i):(blockDivision rs f)
   where (i,f) = splitAt (RS.dataCwCount r) xs

ecBlock :: (RS.RSCode,[GF256]) -> [GF256]
ecBlock (r,s) = RS.ecCodewords r s

assemble :: [[GF256]] -> [[GF256]] -> [GF256]
assemble xs ys = go [] xs ++ go [] ys
   where go [] [] = []
         go xss [] = go [] $ reverse xss
         go xss ([]:yss) = go xss yss
         go xss ((y:ys):yss) = y:(go (ys:xss) yss)


miscodeProtectionCw :: Version -> Int
miscodeProtectionCw v | number v > 4 = 0
                             | otherwise = miscodeProtectionCw4 ! v 

miscodeProtectionCw4 :: Array Version Int
miscodeProtectionCw4 = listArray (MV 1 L,V 4 H)
-- MV 1, 2, 3, 4, V 1, 2, 3, 4  EC
  [   2, 3, 2, 2,   3, 2, 1, 0, -- L
      u, 2, 2, 0,   2, 0, 0, 0, -- M
      u, u, u, 0,   1, 0, 0, 0, -- Q
      u, u, u, u,   1, 0, 0, 0  -- H
  ] where u = undefined