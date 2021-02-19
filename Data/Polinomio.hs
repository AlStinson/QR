{-# LANGUAGE ScopedTypeVariables #-}

module Data.Polinomio where

import Data.Euclideo

data Polinomio a = P a Int (Polinomio a) | C a

instance Eq a => Eq (Polinomio a) where
   (==) (P a n p) (P b m q) = n==m && a==b && p==q
   (==) (C a)     (C b)     = a==b
   (==) _         _         = False
   
instance Functor Polinomio where
   fmap f (C a) = C . f $ a
   fmap f (P a n q) = P (f a) n $ fmap f q
   
   -- f a tiene que ser siempre distinto de 0
   
instance (Eq a, Num a) => Num (Polinomio a) where
   (+) p@(P a n r) q@(P b m s) | n==m = (if (a+b)==0 then id else P (a+b) n) r+s
                               | n<m  = P b m (p+s)
                               | n>m  = P a n (r+q)
   (+) (P a n p)   q                  = P a n (p+q)
   (+) p           (P a n q)          = P a n (p+q)
   (+) (C a)       (C b)              = C $ a+b
   
   (*) p (P b m s) = (go p b m) + p*s
      where go (P a n r) b m = P (a*b) (n+m) (go r b m)
            go (C a) b m | a == 0    = C 0
                         | otherwise = P (a*b) m $ C 0
   (*) p (C 0) = C 0
   (*) (P a n r) q@(C b) = P (a*b) n $ r*q
   (*) (C a) (C b) = C $ a*b
   negate =  fmap negate
   abs = id
   signum _ = 1
   fromInteger = C . fromInteger

instance (Eq a, Fractional a) => Euclideo (Polinomio a) where

   cocienteResto p (C b) = (p*(C . recip $ b),C 0)
   cocienteResto (C a) q = (0, C a)
   cocienteResto p@(P a n r) q@(P b m s) | n>m  = (cocienteParcial + x,y)
                                         | n==m =
       (cocienteIndepe, restoIndepe)
                                         | n<m  = (0, p)
      where cocienteParcial = P (a/b) (n-m) 0
            restoParcial = p-cocienteParcial*q
            (x,y) = cocienteResto restoParcial q
            cocienteIndepe = C $ a/b
            restoIndepe = p-cocienteIndepe*q
           
           
x :: Num a => Polinomio a
x = P 1 1 $ C 0

grado :: Polinomio a -> Int
grado (C _)     = 0
grado (P _ n _) = n

coeficientes :: Num a => Polinomio a -> [a]
coeficientes (C n) = [n]
coeficientes p@(P a n q) = go p n
   where go p@(P a n q) m | m>n = 0:(go p (m-1))
                          | m==n = a:(go q (m-1))
         go (C a) 0 = [a]
         go (C a) m = 0:(go (C a) (m-1))

creaPolinomio :: (Eq a, Num a) => [a] -> Polinomio a
creaPolinomio xs = go xs $ length xs
   where go _      0 = C 0
         go (x:[]) 1 = C x
         go (0:xs) n = go xs (n-1)
         go (x:xs) n = P x (n-1) $ go xs (n-1) 

deListas :: (Eq a, Num a) => [a] -> [Int] -> Polinomio a
deListas [] [] = C 0
deListas (x:xs) (n:ns) = case n of
                           0 -> (C x) + (deListas xs ns)
                           _ -> (P x n 0) + (deListas xs ns)

evalua :: Num a => Polinomio a -> a -> a
evalua p x = go (coeficientes p) 0
   where go [c]    n = n+c
         go (c:cs) n = go cs $ x*(n+c)

esRaiz :: (Eq a,Num a) => Polinomio a -> a -> Bool
esRaiz p = (0==) . (evalua p)

buscaSoluciones :: (Eq a, Num a, Enum a) => Polinomio a -> [a]
buscaSoluciones p = go 1 (grado p)
   where go _ 0 = []
         go r n | esRaiz p r = r:(go (succ r) (n-1))
                | otherwise = go (succ r) n

instance (Show a, Eq a, Num a) => Show (Polinomio a) where
   show x = go x 0
       where go (C 0) 1 = ""
             go p 1 = " + "++go p 0  
             go (C 1) _ = "1"
             go (C n) _ = show n
             go (P a n q) _ = coe a ++ "x" ++ exp n ++ go q 1
                where exp 1 = ""
                      exp n = "^"++show n
                      coe 1 = ""
                      coe n = "("++show n ++ ")*"