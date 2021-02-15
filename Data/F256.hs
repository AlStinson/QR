{-# LANGUAGE FlexibleInstances #-}

module Data.F256 
   (
    module Data.Modulo,
    module Data.Polinomio,

    F256
    
   ) where

import Data.Modulo
import Data.Polinomio
import Data.F2

instance Modulable (Polinomio F2) where
   modulo = P 1 8 $ P 1 4 $ P 1 3 $ P 1 2 $ 1
   generador = M x

type F256 = Modulo (Polinomio F2)

show' :: F256 -> String
show' (M p) = (take (8-length coef ) $ repeat '0') ++ go coef
   where coef = coeficientes p
         go [] = []
         go (0:xs) = '0':(go xs)
         go (1:xs) = '1':(go xs)


elementos :: [F256]
elementos = go generador
   where go x | x == 1 = [1]
              | otherwise = x: (go $ x*generador)



