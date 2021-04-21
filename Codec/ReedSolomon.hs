module Codec.ReedSolomon where

import Data.GF256
import Data.Matrix.Auxiliar
import Data.Either
import Data.Poly


--            RS n k r
data RSCode = RS 
              {
               totalCwCount :: Int,        -- n = numero de palabras totales
               dataCwCount :: Int,         -- k = numero de palabras de datos
               correctionCapacity :: Int -- r = capacidad de coreccion 
              } 
   deriving Show

--   dataCw -> ecCw -> mPCw -> RSCode 
rsCode :: Int -> Int -> Int -> RSCode
rsCode d e m = RS (d+e) d $ div (e-m) 2

ecCwCount :: RSCode -> Int -- Detectores
ecCwCount rs = (totalCwCount rs) - (dataCwCount rs)


polyGen :: RSCode -> Poly GF256
polyGen rs = product [x + (constPoly $ discExp n) 
                          | n<-[0..ecCwCount rs - 1]] 


ecCodewords :: RSCode -> [GF256] -> [GF256] 
ecCodewords rs ps = drop 1 $ coefs $ (monomial 1 (ecCwCount rs) +) $ 
                    polyMod q $ polyGen rs 
   where p = makePoly ps
         q = p*(monomial 1 (ecCwCount rs))
{-         
decode :: RSCode -> Poly GF256 -> Poly GF256
decode rs p = makePoly $ take ((length xs) - (correctionWords rs)) $ xs
   where xs = coefs p 


sindromes :: RSCode -> Polinomio F256 -> [F256]
sindromes rs p = [evalua p $ generador^n | 
                 n<-[1..correctionWords]]

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
-}
   
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