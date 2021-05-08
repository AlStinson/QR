module Codec.QR.Version where

import Data.Array as A
import Codec.QR.Core

import Codec.QR.ErrorCorrection.Level


data Version = V {number :: Int, ecl :: ECLevel} | 
               MV {number :: Int, ecl :: ECLevel}

instance Show Version where
   show (MV n e) = "MV "++show n++" "++show e
   show (V n e)  = "V " ++show n++" "++show e

instance Eq Version where
   (==) (V  n e) (V  m f) = n==m && e==f
   (==) (MV n e) (MV m f) = n==m && e==f
   (==) _        _        = False

instance Ord Version where
   compare (V  _ _) (MV _ _) = GT
   compare (MV _ _) (V  _ _) = LT
   compare v w = case compare (number v) (number w) of
                  GT -> GT
                  LT -> LT
                  EQ -> compare (ecl v) (ecl w)

instance Ix Version where
   range (V  n e, V  m f) = [V  a b | b<-[e..f], a<-[n..m]]
   range (MV n e, MV m f) = [MV a b | b<-[e..f], a<-[n..m]]
   range (MV n e, V  m f) = concat [
                             [MV a b | a<-[n..4]] ++
                             [V  a b | a<-[1..m]]
                            | b<-[e..f]]
   range (_     , _     ) = []

   index p c = go 0 $ range p
      where go n [] = error "Index out of range"
            go n (x:xs) | c==x = n
                        | otherwise = go (n+1) xs

   inRange r v = elem v $ range r 

isMicro :: Version -> Bool
isMicro (MV _ _) = True
isMicro  _       = False

shortLastword :: Version -> Bool
shortLastword = numberVersionCase odd (const False)

versionCase :: (Version -> a) -> (Version -> a) -> Version -> a 
versionCase f g v | isMicro v = f v
                  | otherwise = g v

numberVersionCase :: (Int -> a) -> (Int -> a) -> Version -> a
numberVersionCase f g = versionCase (f . number) (g . number)

kindVersionCase :: a -> a -> Version -> a
kindVersionCase f g = versionCase (const f) (const g)

sizeVersionCase ::  (Int -> a) -> (Int -> a) -> Version -> a
sizeVersionCase f g = versionCase (f . size) (g . size)

shortLastwordVersionCase :: (Version -> a) -> (Version -> a) -> Version -> a
shortLastwordVersionCase f g = versionCase f' g
   where f' v | shortLastword v = f v
              | otherwise = g v

size :: Version -> Int
size = numberVersionCase sizeMV sizeV 

sizeV :: Int -> Int
sizeV x = 17+4*x

sizeMV :: Int -> Int
sizeMV x = 9+2*x

micros :: ECLevel -> [Version]
micros e = range (MV 1 e, MV 4 e)

ecLevelMinVersion :: ECLevel -> Version
ecLevelMinVersion ecl = case ecl of
   L -> MV 1 L
   M -> MV 2 M
   Q -> MV 4 Q
   H -> V  1 H

ecBlocks :: Array Version Int 
ecBlocks = A.listArray (MV 1 L, V 40 H) 
-- MV 1, 2, 3, 4, V 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40  EC 
  [   1, 1, 1, 1,   1, 1, 1, 1, 1, 2, 2, 2, 2,  4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25, -- L
      u, 1, 1, 1,   1, 1, 1, 2, 2, 4, 4, 4, 5,  5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49, -- M
      u, u, u, 1,   1, 1, 2, 2, 4, 4, 6, 6, 8,  8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68, -- Q
      u, u, u, u,   1, 1, 2, 4, 4, 4, 5, 6, 8,  8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81  -- H
  ] where u = undefined

ecCwPerBlock :: Array Version Int
ecCwPerBlock = A.listArray (MV 1 L,V 40 H)
-- MV 1, 2, 3,  4, V  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40  EC
  [   2, 5, 6,  8,    7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, -- L
      u, 6, 8, 10,   10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, -- M
      u, u, u, 13,   13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, -- Q
      u, u, u,  u,   17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30  -- H
  ] where u = undefined

{-
validVersion :: Version -> Bool
validVersion = numberVersionCase f g
   where f x = x>=1 && x<=4
         g x = x>=1 && x<=40
-}