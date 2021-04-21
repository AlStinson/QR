module Data.Matrix.Auxiliar 
   (

    module Data.Matrix,

    include,
    include1,
    menorInvertible

   ) where

import Data.Matrix
import Data.Maybe


-- Inserta b en a en la posicion (i,j) sustituyendo sus valores
include :: (Int,Int) -> Matrix a -> Matrix a -> Matrix a
include (i,j) b a = foldl f a [(n, m) | n<-[1..nrows b], m<-[1..ncols b]]
   where f qr (n,m) = setElem (b ! (n,m)) (i+n-1,j+m-1) qr

include1 :: Matrix a -> (Int,Int,Matrix a) -> Matrix a
include1 a (i,j,b) = include (i,j) b a

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
                              



