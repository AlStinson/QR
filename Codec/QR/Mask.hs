module Codec.QR.Mask 
   (
    Mask,
    applyMask
   ) where

import Codec.QR.Module
import Codec.QR.Version
import Codec.QR.Core
import Codec.QR.QR

type Mask = Int

applyMask :: Int -> Version -> QR -> QR
applyMask n v = fArray f
   where t = reservedMod v
         f p | t ! p = id
             | otherwise = xor (mask n v p)

mask :: Int -> Version -> (Module -> Bool)
mask n = kindVersionCase (maskMV n) (maskV n)

maskV :: Int  -> (Module -> Bool)
maskV n = case n of
  0 -> (\(i,j) -> mod (i+j) 2 == 0)
  1 -> (\(i,j) -> mod i 2 == 0)
  2 -> (\(i,j) -> mod j 3 == 0)
  3 -> (\(i,j) -> mod (i+j) 3 == 0)
  4 -> (\(i,j) -> mod ((div i 2)+(div j 3)) 2 == 0)
  5 -> (\(i,j) -> (mod (i*j) 2) + (mod (i*j) 3) == 0)
  6 -> (\(i,j) -> mod ((mod (i*j) 2) + (mod (i*j) 3)) 2 == 0)
  7 -> (\(i,j) -> mod ((mod (i+j) 2) + (mod (i*j) 3)) 2 == 0)
  _ -> error $ "Undefined mask: "++ show n


maskMV :: Int -> (Module -> Bool)
maskMV n = case n of
  0 -> maskV 1
  1 -> maskV 4
  2 -> maskV 6
  3 -> maskV 7
  _ -> error $ "undefined micro mask: "++show n

