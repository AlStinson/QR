module Data.BitString where

import Data.Word

type BitString = [Bool]

integralToBitString :: (Integral a) => a -> Int  -> BitString
integralToBitString x n = go x n []
   where go 0 0 xs = xs
         go _ 0 xs = error "integralToBitString necesita mas espacio para expresar un numero"
         go x n xs = go d (n-1) $ (m==1):xs
            where (d,m) = divMod x 2


bitStringToNum :: (Num a) => BitString -> a
bitStringToNum bs = go bs 0
   where go [] a = a
         go (b:bs) a = go bs (2*a+x)
            where x | b         = 1
                    | otherwise = 0

-- *Main> quickCheck (\x -> fromBitString (toBitString (abs x) (abs x)) == (abs x))
-- +++ OK, passed 100 tests.