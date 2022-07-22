module Codec.QR.Mask where

import Codec.QR.Module
import Codec.QR.Version
import Codec.QR.QR
import Data.Bits (xor)
import Data.Array (Array)

type Mask = Int

applyMask :: Int -> Version -> QR -> QR
applyMask n v qr = array b [(i, maskModule n v qr t i) | i<-range b]
   where b = bounds qr
         t = reservedMod v

maskModule :: Int -> Version -> QR -> Array Module Bool -> Module -> Bool
maskModule n v qr t p = (if t ! p then id else xor $ mask n v p) $ qr ! p


mask :: Int -> Version -> Module -> Bool
mask n = kindVersionCase (maskMV n) (maskV n)

maskV :: Int  -> Module -> Bool
maskV 0 (i,j) = mod (i+j) 2 == 0
maskV 1 (i,j) = mod i 2 == 0
maskV 2 (i,j) = mod j 3 == 0
maskV 3 (i,j) = mod (i+j) 3 == 0
maskV 4 (i,j) = mod ((div i 2)+(div j 3)) 2 == 0
maskV 5 (i,j) = (mod (i*j) 2) + (mod (i*j) 3) == 0
maskV 6 (i,j) = mod ((mod (i*j) 2) + (mod (i*j) 3)) 2 == 0
maskV 7 (i,j) = mod ((mod (i+j) 2) + (mod (i*j) 3)) 2 == 0
maskV n _ = error $ "Undefined mask: "++ show n


maskMV :: Int -> Module -> Bool
maskMV 0 = maskV 1
maskMV 1 = maskV 4
maskMV 2 = maskV 6
maskMV 3 = maskV 7
maskMV n = const $ error $ "Undefined micro mask: "++show n

maxMask :: Version -> Mask
maxMask = kindVersionCase 3 7
