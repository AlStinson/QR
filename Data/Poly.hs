{-# LANGUAGE ScopedTypeVariables #-}

module Data.Poly where

data Poly a = P a Int (Poly a) | C a

instance Eq a => Eq (Poly a) where
   (==) (P a n p) (P b m q) = n==m && a==b && p==q
   (==) (C a)     (C b)     = a==b
   (==) _         _         = False
   
instance Functor Poly where
   fmap f (C a) = C . f $ a
   fmap f (P a n q) = P (f a) n $ fmap f q
   
   -- f a tiene que ser siempre distinto de 0
   
instance (Eq a, Num a) => Num (Poly a) where
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

distance :: (Bounded a, Enum a) => a -> Int
distance a = fromEnum mx - fromEnum mn + 1
   where mx = maxBound `asTypeOf` a
         mn = minBound `asTypeOf` a

instance forall a. (Eq a, Num a, Bounded a, Enum a) => Enum (Poly a) where
   toEnum n | n<0 = error "Enum in poly needs no negative integer"
            | otherwise = makePoly $ reverse $ go n
      where go 0 = []
            go n = let (d,m) = divMod n $ distance (0::a)
                   in (minBound + toEnum m):(go d)
   fromEnum p = eval (fmap fromEnum p) (distance (0::a))

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
polyDiv p q = c
   where (c,_) = polyDivMod p q

polyMod :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
polyMod p q = r
   where (_,r) = polyDivMod p q
           
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
coefs (C n) = [n]
coefs p@(P a n q) = go p n
   where go p@(P a n q) m | m>n = 0:(go p (m-1))
                          | m==n = a:(go q (m-1))
         go (C a) 0 = [a]
         go (C a) m = 0:(go (C a) (m-1))

makePoly :: (Eq a, Num a) => [a] -> Poly a
makePoly xs = go xs $ length xs
   where go _      0 = C 0
         go (x:[]) 1 = C x
         go (0:xs) n = go xs (n-1)
         go (x:xs) n = P x (n-1) $ go xs (n-1) 

fromLists :: (Eq a, Num a) => [a] -> [Int] -> Poly a
fromLists [] [] = C 0
fromLists (x:xs) (n:ns) = case n of
                           0 -> (C x) + (fromLists xs ns)
                           _ -> (P x n 0) + (fromLists xs ns)

eval :: Num a => Poly a -> a -> a
eval p x = go (coefs p) 0
   where go [c]    n = n+c
         go (c:cs) n = go cs $ x*(n+c)

isZero :: (Eq a,Num a) => Poly a -> a -> Bool
isZero p = (0==) . (eval p)

searchZeros :: (Eq a, Num a, Enum a) => Poly a -> [a]
searchZeros p = searchZerosBy p succ 1

searchZerosBy :: (Eq a, Num a) => Poly a -> (a -> a) -> a -> [a]
searchZerosBy p s i = go i (grade p)
   where go _ 0 = []
         go r n | isZero p r = r:(go r' (n-1))
                | otherwise  = go r' n
            where r' = s r

instance (Show a, Eq a, Num a) => Show (Poly a) where
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