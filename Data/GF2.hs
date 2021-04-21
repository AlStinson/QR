module Data.Bool.Instances where

import Data.Bits (xor)
import Data.Ratio (numerator, denominator)

instance Num Bool where
   (+) = xor
   (-) = xor
   (*) = (&&) 
   fromInteger x = mod x 2 == 1
   abs = id
   signum = id

instance Fractional Bool where
   recip x = if x then x else error "divide by zero"
   fromRational q = n/d 
      where n = fromInteger $ numerator q
            d = fromInteger $ denominator q

instance Real Bool where
   toRational x = if x then 1 else 0