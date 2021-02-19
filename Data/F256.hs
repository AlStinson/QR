{-# LANGUAGE FlexibleInstances #-}

module Data.F256 
   (
    module Data.Modulo,
    module Data.Polinomio,

    F256,
    fromInt
    
   ) where

import Data.Modulo
import Data.Polinomio
import Data.F2
import Data.Maybe

instance Modulable (Polinomio F2) where
   modulo = P 1 8 $ P 1 4 $ P 1 3 $ P 1 2 $ 1
   generador = M x

instance {-# Overlaps #-} Show F256 where
   show = show . toInt

type F256 = Modulo (Polinomio F2)

elementos :: [F256]
elementos = go (generador::F256)
   where go :: F256 -> [F256]
         go x | x == 1 = [1]
              | otherwise = x: (go $ x*generador)

toInt :: F256 -> Int
toInt (M p) = evalua (fmap (\(M x)-> x) p) 2

fromInt :: Int -> F256
fromInt = reduce . creaPolinomio . reverse . go . (\x -> mod x 256)
   where go 0 = [0]
         go 1 = [1]
         go x = (reduce m):(go d)
            where (d,m) = divMod x 2 

instance Bounded F256 where
   minBound = 0
   maxBound = fromInt 255

