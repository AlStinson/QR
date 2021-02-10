module Codec.QR.Mask where

mask :: Int  -> ((Int,Int) -> Bool)
mask 0 = (\(i,j) -> mod (i+j) 2 == 0)
mask 1 = (\(i,j) -> mod i 2 == 0)
mask 2 = (\(i,j) -> mod j 3 == 0)
mask 3 = (\(i,j) -> mod (i+j) 3 == 0)
mask 4 = (\(i,j) -> mod ((div i 2)+(div j 3)) 2 == 0)
mask 5 = (\(i,j) -> (mod (i*j) 2) + (mod (i*j) 3) == 0)
mask 6 = (\(i,j) -> mod ((mod (i*j) 2) + (mod (i*j) 3)) 2 == 0)
mask 7 = (\(i,j) -> mod ((mod (i+j) 2) + (mod (i*j) 3)) 2 == 0)
mask x = error $ "Undefined mask: "++ show x

maskMicro :: Int -> ((Int,Int) -> Bool)
maskMicro 0 = mask 1
maskMicro 1 = mask 4
maskMicro 2 = mask 6
maskMicro 3 = mask 7
maskMicro x = error $ "undefined micro mask: "++show x
