module Data.BitString where

import Data.Bits (xor)
import Data.Ratio (numerator,denominator)
import Data.Function (on)

type BitString = [Bool]

integralsToBitString :: (Integral a) => [Int] -> [a] -> BitString
integralsToBitString ns = concat . zipWith (integralToBitString) ns

integralToBitString :: (Integral a) => Int -> a  -> BitString
integralToBitString n x = go n x []
   where go 0 0 xs = xs
         go 0 _ xs = error "integralToBitString needs more bits"
         go n x xs = go (n-1) d $ (m==1):xs
            where (d,m) = divMod x 2


bitStringToNums :: (Num a) => [Int] -> BitString -> [a]
bitStringToNums ns xs = go ns xs $ length xs
   where go (n:ns) xs m 
          | m == 0    = []
          | m <  n    = [bitStringToNum $ xs ++ take (n-m)(repeat False)]
          | otherwise = let (a,b) = splitAt n xs 
                        in (bitStringToNum a):(go ns b (m-n))
                 
bitStringToNum :: (Num a) => BitString -> a
bitStringToNum = foldl (\a b ->  2*a+ if b then 1 else 0) 0

count :: Num a => BitString -> a
count = foldl (\x y -> x + if y then 1 else 0) 0

-- Instances

instance Num Bool where
   (+) = xor
   (-) = xor
   (*) = (&&) 
   fromInteger x = mod x 2 == 1
   abs = id
   signum = id

instance Fractional Bool where
   recip x = if x then x else error "divide by zero"
   fromRational q = on (/) fromInteger (numerator q) (denominator q) 
