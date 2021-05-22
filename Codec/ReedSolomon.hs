module Codec.ReedSolomon where

import Data.GF256
import Data.Matrix hiding (fromLists)
import Data.Poly
import Data.Maybe


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
         
decode :: RSCode -> [GF256] -> Maybe [GF256]
decode rs xs = do restored <- corrigeErrores rs $ makePoly xs
                  return $ take (dataCwCount rs) $ drop 1 $ coefs $ 
                           monomial 1 (totalCwCount rs) + restored


sindromes :: RSCode -> Poly GF256 -> [GF256]
sindromes rs p = [eval p $ discExp n | n<-[0..ecCwCount rs -1]]

corrigeErrores :: RSCode -> Poly GF256 -> Maybe (Poly GF256)
corrigeErrores rs p | all (0==) s = Just p
                    | numErrores > correctionCapacity rs = Nothing
                    | otherwise = Just $ p - (fromLists errores pos)
   where s = sindromes rs p
         expPos = expPosErrores rs s
         pos = map discLog expPos
         numErrores = length expPos
         errores = toList $ m*(fromList numErrores 1 s)
            where (Right m) = inverse $ matrix numErrores numErrores 
                              (\(x,y) -> (expPos !! (y-1))^(x-1))

expPosErrores :: RSCode -> [GF256] -> [GF256]
expPosErrores rs s = searchZerosBy ((discExp 1)*) 1 . makePoly . (1:) . 
                   reverse . toList $ inv * b 
   where d1 = div (ecCwCount rs) 2
         d2 = nrows inv
         inv = menorInvertible $ (matrix d1 d1) $ 
               (\(x,y) -> (s !! (x+y-2)))
         b = matrix d2 1 (\(x,_) -> -(s !!(d2+x-1)))

menorInvertible :: (Eq a,Fractional a) => Matrix a -> Matrix a
menorInvertible a = sup 1 (a <|> (identity n))
   where n = nrows a 
         sup m a | m > n     = inf n n a
                 | isJust k  = sup (m+1) $ limpiaAbajo n m $ 
                               if m==k' then a else combineRows m 1 k' a
                 | otherwise = inf (m-1) (m-1) a
            where k = encuentraNoNulo n m a
                  k' = fromJust k
         inf 0 m = submatrix 1 m (n+1) (n+m)
         inf k m = (inf (k-1) m) . (limpiaArriba k) 


-- Hace cero todos los elementos debajo a_{m,m} y uno dicho elemento
-- donde la matriz a tiene n filas
limpiaAbajo :: Fractional a => Int -> Int -> Matrix a -> Matrix a
limpiaAbajo n m a = go (m+1) $ scaleRow (recip $ a ! (m,m)) m a
   where go k a | k > n     = a
                | otherwise = go (k+1) $ combineRows k (negate $ a ! (k,m)) m a 
                 

limpiaArriba :: Fractional a => Int -> Matrix a -> Matrix a
limpiaArriba m a = go (m-1) a 
   where go 0 a = a
         go k a = go (k-1) $ combineRows k (negate $ a ! (k,m)) m a 

--Encuentra el primer elemento no nulo debajo de a_{m,m} y suma esa fila
--a la m-esima fila en una matriz de n filas 
encuentraNoNulo :: (Eq a, Num a) => Int -> Int -> Matrix a -> Maybe Int
encuentraNoNulo n m a = go m a
   where go k a | k>n = Nothing
                | otherwise = if (a!(k,m))/=0 then Just k else go (k+1) a

   
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
--AQUI SE QUEDA PILLADO PORQUE EL POLINOMIO DE LAS POSICIONES NO TIENE SOLUCIONES
{-
*Codec.ReedSolomon> let rs = rsCode 4 6 1
*Codec.ReedSolomon> let p = makePoly [1,2,3,4,204,15,200,245,109,200]

-}