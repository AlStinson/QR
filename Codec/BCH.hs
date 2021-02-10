module Codec.BCH where

import Data.F256

polGenerador :: Int -> Polinomio F256
polGenerador 1 = x + C generador
polGenerador n = (P 1 1 $ C generador^n)* (polGenerador (n-1))

codifica :: Int -> Polinomio F256 -> Polinomio F256
codifica n p = q-(resto q $ polGenerador n) 
   where q = p*(P 1 n 0)