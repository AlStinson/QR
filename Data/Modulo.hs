{-# LANGUAGE ScopedTypeVariables #-}

module Data.Modulo 
   (
    module Data.Euclideo,
    
    Modulo(..),
    Modulable(..)
   ) where

import Data.Euclideo
import Data.Ratio

data Modulo a = M a

class (Eq a, Euclideo a) => Modulable a where
   modulo :: a
   reduce :: a -> Modulo a
   
   reduce x = M $ resto x modulo

instance Modulable a => Eq (Modulo a) where
   (==) (M x) (M y) = x == y

instance Modulable a => Num (Modulo a) where
   (+) (M x) (M y) = reduce $ x+y
   (-) (M x) (M y) = reduce $ x-y
   (*) (M x) (M y) = reduce $ x*y
   abs = id
   signum (M x) = if x==0 then 0 else 1
   fromInteger = reduce . fromInteger
   
instance Modulable a => Fractional (Modulo a) where
   recip x = go x 1
      where go 0 z = error "Elemento no invertible"
            go 1 z = z
            go y z = go (x*y) y
   fromRational z = (reduce . fromInteger . numerator) z  / 
                    (reduce . fromInteger . denominator) z

instance (Show a, Modulable a) => Show (Modulo a) where
   show (M a) = show a ++ " mod " ++ show (modulo :: a)