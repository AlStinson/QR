module Codec.QR.Version where

import Data.Ix

data Version = V {number :: Int} |  MV {number :: Int}
   deriving (Eq)

instance Show Version where
   show x = (kindVersionCase "MV " "V  " x) ++ (show $ number x)

instance Ord Version where
   compare (V  _) (MV _) = GT
   compare (MV _) (V  _) = LT
   compare v w = compare (number v) (number w)

instance Ix Version where
   range (V  n, V  m) = [V  a | a<-[n..m]]
   range (MV n, MV m) = [MV a | a<-[n..m]]
   range (MV n, V  m) = [MV a | a<-[n..4]] ++
                        [V  a | a<-[1..m]]
   range (_   , _   ) = []
   index p c = go 0 $ range p 
      where go n [] = error "Index out of range"
            go n (x:xs) | c==x = n
                        | otherwise = go (n+1) xs
   inRange (a,b) v = a <= v && v<= b


isMicro :: Version -> Bool
isMicro MV{} = True
isMicro  _     = False

versionCase :: (Version -> a) -> (Version -> a) -> Version -> a 
versionCase f g v | isMicro v = f v
                  | otherwise = g v

numberVersionCase :: (Int -> a) -> (Int -> a) -> Version -> a
numberVersionCase f g = versionCase (f . number) (g . number)

kindVersionCase :: a -> a -> Version -> a
kindVersionCase f g = versionCase (const f) (const g)

sizeVersionCase ::  (Int -> a) -> (Int -> a) -> Version -> a
sizeVersionCase f g = numberVersionCase (f . sizeMV) (g . sizeV)

shortLastwordVersionCase :: (Version -> a) -> (Version -> a) -> Version -> a
shortLastwordVersionCase f g = versionCase f' g
   where f' v | shortLastword v = f v
              | otherwise = g v

-- Como la array empieza por (0,0), el ultimo modulo es uno menos que
-- lo que pone en el standard

size :: Version -> Int
size = numberVersionCase sizeMV sizeV

sizeV :: Int -> Int
sizeV x = 16+4*x

sizeMV :: Int -> Int
sizeMV x = 8+2*x

unSize :: Int -> Maybe Version 
unSize x | x<=16     = if and [mMV==0,1<=dMV,4 >=dMV] 
                       then Just $ MV dMV else Nothing
         | otherwise = if and [mV ==0,1<=dV ,40>=dV ] 
                       then Just $ V  dV  else Nothing 
   where (dMV,mMV) = divMod (x-8) 2
         (dV,mV) = divMod (x-16) 4

unsafeUnSize :: Int -> Version 
unsafeUnSize x = if x<=16 then MV $ div (x-8) 2 else V $ div (x-16) 4

micros :: [Version]
micros = range (MV 1, MV 4)

shortLastword :: Version -> Bool
shortLastword = numberVersionCase odd (const False)