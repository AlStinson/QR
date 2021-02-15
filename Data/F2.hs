module Data.F2 where

import Data.Modulo

instance Modulable Int where
   modulo = 2
   generador = 1

type F2 = Modulo Int