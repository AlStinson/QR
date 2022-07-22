module Data.Poly where

data Poly a = P a Int (Poly a) | C a

instance Functor Poly where
   fmap f (C a) = C . f $ a
   fmap f (P a n q) = P (f a) n $ fmap f q
   
instance (Eq a, Num a) => Num (Poly a) where
   (+) p@(P a n r) q@(P b m s) | n==m = (if (a+b)==0 then id 
                                         else P (a+b) n) r+s
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

polyDivMod :: (Eq a, Fractional a) => Poly a -> Poly a -> (Poly a, Poly a)
polyDivMod p (C b) = (p*(C . recip $ b),C 0)
polyDivMod (C a) q = (0, C a)
polyDivMod p@(P a n r) q@(P b m s) | n>m  = (cocienteParcial + x,y)
                                   | n==m = (cocienteIndepe, restoIndepe)
                                   | n<m  = (0, p)
      where cocienteParcial = P (a/b) (n-m) 0
            restoParcial = p-cocienteParcial*q
            (x,y) = polyDivMod restoParcial q
            cocienteIndepe = C $ a/b
            restoIndepe = p-cocienteIndepe*q
           
polyDiv :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
polyDiv p q = c where (c,_) = polyDivMod p q

polyMod :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
polyMod p q = r where (_,r) = polyDivMod p q
        
x :: Num a => Poly a
x = P 1 1 $ C 0

constPoly :: Num a => a -> Poly a
constPoly = C

monomial :: Num a => a -> Int -> Poly a
monomial a n = P a n $ C 0

grade :: Poly a -> Int
grade (C _)     = 0
grade (P _ n _) = n

coefs :: Num a => Poly a -> [a]
coefs p = go p $ grade p
   where go (P a n q) m = replicate (m-n) 0 ++ a:(go q (n-1))
         go (C a) m = (replicate m 0) ++ [a]

coefsn :: (Eq a, Num a) => Int -> Poly a -> [a]
coefsn n p = replicate (n - grade p - 1) 0 ++ coefs p

makePoly :: (Eq a, Num a) => [a] -> Poly a
makePoly xs = go xs $ length xs
   where go _      0 = C 0
         go (x:[]) 1 = C x
         go (0:xs) n = go xs (n-1)
         go (x:xs) n = P x (n-1) $ go xs (n-1) 

fromLists :: (Eq a, Num a) => [a] -> [Int] -> Poly a
fromLists xs = sum . zipWith f xs
   where f x n = if n==0 then C x else P x n 0 

eval :: Num a => Poly a -> a -> a
eval p x = foldl (\a c -> x*a+c) 0 $ coefs p

isZero :: (Eq a,Num a) => Poly a -> a -> Bool
isZero p = (0==) . (eval p)

searchZerosBy :: (Eq a, Num a) => (a -> a) -> a -> Poly a -> [a]
searchZerosBy s i p = go (grade p) $ i:(nexts $ s i)
   where go 0 _  = []
         go _ [] = []
         go n (x:xs) = if isZero p x then x:(go (n-1) xs) else go n xs
         nexts y = if y==i then [] else y:(nexts $ s y)
