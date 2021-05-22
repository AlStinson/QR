module Data.BitString where

type BitString = [Bool]

integralsToBitString :: (Integral a) => [Int] -> [a] -> BitString
integralsToBitString ns = concat . zipWith (integralToBitString) ns

integralToBitString :: (Integral a) => Int -> a  -> BitString
integralToBitString n x = go n x []
   where go 0 0 xs = xs
         go 0 _ xs = error "integralToBitString necesita mas espacio"
         go n x xs = go (n-1) d $ (m==1):xs
            where (d,m) = divMod x 2

bitStringToNums :: (Num a) => [Int] -> BitString -> [a]
bitStringToNums ns xs = go ns xs $ length xs
   where go (n:ns) xs m | m == 0    = []
                        | m <  n    = [bitStringToNum $ xs ++ 
                                       take (n-m)(repeat False)]
                        | otherwise = let (a,b) = splitAt n xs 
                                      in (bitStringToNum a):(go ns b (m-n))
                 

bitStringToNum :: (Num a) => BitString -> a
bitStringToNum bs = go bs 0
   where go [] a = a
         go (b:bs) a = go bs (2*a+x)
            where x | b         = 1
                    | otherwise = 0
