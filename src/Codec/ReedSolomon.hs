module Codec.ReedSolomon where

import Data.GF256
import Data.Matrix hiding (fromLists)
import Data.Poly
import Data.Maybe


--            RS n k r

data RSCode = RS { rsTotalCwCount :: Int, 
                   rsDataCwCount :: Int,
                   rsCorrectionCapacity :: Int} 

rsEcCwCount :: RSCode -> Int 
rsEcCwCount rs = (rsTotalCwCount rs) - (rsDataCwCount rs)

--   dataCw -> ecCw -> mPCw -> RSCode 

rsCode :: Int -> Int -> Int -> RSCode
rsCode d e m = RS (d+e) d $ div (e-m) 2


ecCodewords :: RSCode -> [GF256] -> [GF256] 
ecCodewords rs ps = coefsn e $ polyMod ((makePoly ps)*(monomial 1 e)) $ 
                               product [x+ constPoly s | s<-solutions e]  
   where e = rsEcCwCount rs

solutions :: Int -> [GF256]
solutions n = [discExp k | k<-[0 .. n-1]] 


rsDecode :: RSCode -> [GF256] -> Maybe [GF256]
rsDecode rs xs = do restored <- restore rs $ makePoly xs
                    return $ take (rsDataCwCount rs) $ 
                           coefsn (rsTotalCwCount rs) restored

restore :: RSCode -> Poly GF256 -> Maybe (Poly GF256)
restore rs p | all (0==) syn = Just p
             | isNothing mExpPos = Nothing
             | numErrors > rsCorrectionCapacity rs = Nothing
             | otherwise = Just $ p - (fromLists errors $ map discLog expPos)
   where syn = [eval p s | s<-solutions $ rsEcCwCount rs]
         mExpPos = expPosErrors rs syn
         expPos = fromJust mExpPos
         numErrors = length expPos
         errors = toList $ m*(fromList numErrors 1 syn)
            where (Right m) = inverse $ matrix numErrors numErrors 
                              (\(x,y) -> (expPos !! (y-1))^(x-1))

expPosErrors :: RSCode -> [GF256] -> Maybe [GF256]
expPosErrors rs s = if length zeros < d2 then Nothing else Just zeros 
   where d1 = div (rsEcCwCount rs) 2
         d2 = nrows inv
         inv = minorInvertible $ (matrix d1 d1) $ 
               (\(x,y) -> (s !! (x+y-2)))
         b = matrix d2 1 (\(x,_) -> -(s !!(d2+x-1)))
         zeros = searchZerosBy (2*) 1 $ makePoly $ (1:) $ 
                 reverse $ toList $ inv * b 

minorInvertible :: (Eq a,Fractional a) => Matrix a -> Matrix a
minorInvertible a = sup 1 (a <|> (identity n))
   where n = nrows a 
         sup m a | m > n     = inf n n a
                 | isJust k  = sup (m+1) $ cleanDown n m $ 
                               if m==k' then a else combineRows m 1 k' a
                 | otherwise = inf (m-1) (m-1) a
            where k = findNoNull n m a
                  k' = fromJust k
         inf 0 m = submatrix 1 m (n+1) (n+m)
         inf k m = (inf (k-1) m) . (cleanUp k) 


-- Hace cero todos los elementos debajo a_{m,m} y uno dicho elemento
-- donde la matriz a tiene n filas

cleanDown :: Fractional a => Int -> Int -> Matrix a -> Matrix a
cleanDown n m a = go (m+1) $ scaleRow (recip $ a ! (m,m)) m a
   where go k a | k > n     = a
                | otherwise = go (k+1) $ combineRows k (negate $ a ! (k,m)) m a 
                 

cleanUp :: Fractional a => Int -> Matrix a -> Matrix a
cleanUp m a = go (m-1) a 
   where go 0 a = a
         go k a = go (k-1) $ combineRows k (negate $ a ! (k,m)) m a 

--Encuentra el primer elemento no nulo debajo de a_{m,m} y suma esa fila
--a la m-esima fila en una matriz de n filas 

findNoNull :: (Eq a, Num a) => Int -> Int -> Matrix a -> Maybe Int
findNoNull n m a = go m a
   where go k a | k>n = Nothing
                | otherwise = if (a!(k,m))/=0 then Just k else go (k+1) a

   
-----------------------------------------------------------------------------
