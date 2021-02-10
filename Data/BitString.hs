module Data.BitString where

type BitString = [Bool]

toBitString :: (Integral a) => a -> Int  -> BitString
toBitString x n = go x n []
   where go _ 0 xs = xs
         go x n xs = go d (n-1) ((m==1):xs)
            where (d,m) = divMod x 2

--toBitString no comprueba si necesitas mas digitos de los que le das

fromBitString :: (Num a) => BitString -> a
fromBitString bs = go bs 0
   where go [] a = a
         go (b:bs) a = go bs (2*a+x)
            where x | b         = 1
                    | otherwise = 0

-- *Main> quickCheck (\x -> fromBitString (toBitString (abs x) (abs x)) == (abs x))
-- +++ OK, passed 100 tests.