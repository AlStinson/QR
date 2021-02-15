module Codec.ReedSolomon where

import Data.F256
import Data.Euclideo
import Data.Matrix
import Data.Either
import Data.Maybe


--            RS n k r
data RSCode = RS 
              {
               palabras :: Int,    -- n = numero de palabras totales
               datos :: Int,       -- k = numero de palabras de datos
               correcciones :: Int -- r = capacidad de coreccion
              } 

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
corrigeErrores rs p | all (0==) s = p
                    | otherwise = corrige p (map toEnum posErrores) errores
   where s = sindromes rs p
         go c = (try c ) .  inverse . (matrix c c) $
                (\(x,y) -> (-1)^(x+y)*(s !! (x+y-2)))
         try c (Left _) = go $ c-1
         try c (Right m) = m*(matrix c 1 (\(x,_) -> -(s !! c+x-1)))
         posErrores = buscaSoluciones $ creaPolinomio $ go $
                      correcciones r
         numErrores = length posErrores
         sindromesM = fromList numErrores 1 s 
         errores = fromJust $ sisLineal $ matrix numErrores numErrores 
                   (\(x,y) -> (posErrores !! y)^x) (fromList numErrores
                                                                 1 s)
         corrige p [] [] = p
         corrige p (n:ns) (a:as) = corrige (p-(P a n 0)) ns as

sisLineal :: Matrix a -> Matrix a -> Maybe (Matrix a)
sisLineal a b = go (inverse a)
   where go (Left _) = Nothing
         go (Right m) = Just m*a
   
