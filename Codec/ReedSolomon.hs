module Codec.ReedSolomon where

import Data.F256
import Data.Euclideo
import Data.Matrix
import Data.Either
import Data.BuscaInversa


--            RS n k r
data RSCode = RS 
              {
               palabras :: Int,    -- n = numero de palabras totales
               datos :: Int,       -- k = numero de palabras de datos
               correcciones :: Int -- r = capacidad de coreccion
              } 
   deriving Show

detectores :: RSCode -> Int
detectores rs = (palabras rs) - (datos rs)


polGenerador :: RSCode -> Polinomio F256
polGenerador = go . detectores where
   go 1 = x + C generador
   go n = (P 1 1 $ C generador^n)* (go (n-1))

codifica :: RSCode -> Polinomio F256 -> Polinomio F256
codifica rs p = q-(resto q $ polGenerador rs) 
   where q = p*(P 1 (detectores rs) 0)

decodifica :: RSCode -> Polinomio F256 -> Polinomio F256
decodifica rs p = creaPolinomio $ take ((length xs) - (detectores rs)) $ xs
   where xs = coeficientes p 

sindromes :: RSCode -> Polinomio F256 -> [F256]
sindromes rs p = [evalua p $ generador^n | n<-[1..2*correcciones rs]]

corrigeErrores :: RSCode -> Polinomio F256 -> Polinomio F256
corrigeErrores rs p = p- if all (0==) s then 0 else (deListas errores pos)
   where s = sindromes rs p
         expPos = expPosErrores rs s
         pos = map fromEnum expPos
         numErrores = length expPos
         errores = toList $ m*(fromList numErrores 1 s)
            where (Right m) = inverse $ matrix numErrores numErrores 
                              (\(x,y) -> (expPos !! (y-1))^x)

expPosErrores :: RSCode -> [F256] -> [F256]
expPosErrores rs s = buscaSoluciones . creaPolinomio . (1:) . 
                   reverse . toList $ inv * b 
   where d1 = correcciones rs
         d2 = nrows inv
         inv = menorInvertible $ (matrix d1 d1) $ 
               (\(x,y) -> (s !! (x+y-2)))
         b = matrix d2 1 (\(x,_) -> -(s !!(d2+x-1)))
   
-----------------------------------------------------------------------------

-- Ejemplo 

{-

rs = RS 7 3 2
mensaje :: Polinomio F256
mensaje = creaPolinomio [1,reduce x,reduce (x+1)]
cifrado = codifica rs mensaje

errorr = creaPolinomio [reduce x, 0,0,0,reduce (x^2)]
errado = cifrado +errorr
s = sindromes rs errado
expPos = expPosErrores rs s
pos = map fromEnum expPos
numErrores = length expPos
errores = toList $ m*(fromList numErrores 1 s)
   where (Right m) = inverse $ matrix numErrores numErrores 
                     (\(x,y) -> (expPos !! (y-1))^x)

-}