module Data.Modulo 
   (    
    Modulo(..),
    Modulable(..)

   
   ) where

import Data.Euclideo
import Data.Ratio

data Modulo a = M a

class (Eq a, Euclideo a) => Modulable a where
   modulo :: a
   generador :: Modulo a

   reduce :: a -> Modulo a
   reduce x = M $ resto x modulo

instance Modulable a => Eq (Modulo a) where
   (==) (M x) (M y) = x == y

instance Modulable a => Num (Modulo a) where
   (+) (M x) (M y) = reduce $ x+y
   (-) (M x) (M y) = reduce $ x-y
   (*) (M x) (M y) = reduce $ x*y
   abs = id
   signum _ = 1
   fromInteger = reduce . fromInteger
   
instance Modulable a => Fractional (Modulo a) where
   recip x = go (x*x) x
      where go 0 z = error "Elemento no invertible"
            go 1 z = z
            go y z | y==x      = go 0 z
                   | otherwise = go (x*y) y
   fromRational z = (reduce . fromInteger . numerator) z  / 
                    (reduce . fromInteger . denominator) z
   
instance Modulable a => Enum (Modulo a) where
   succ = (*) generador
   pred = (*) (recip generador)
   enumFrom x = x:(enumFrom . succ $ x)
   toEnum = (^) generador
   fromEnum 1 = 0
   fromEnum x = go generador 1
      where go 1 _ = error "Elemento no enumerable"
            go y n | x==y = n
                   | otherwise = go (succ y) (n+1) 


instance (Show a, Modulable a) => Show (Modulo a) where
   show (M a) = show a ++ " mod " ++ show (modulo `asTypeOf` a)