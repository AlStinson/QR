module Codec.QR.Score  where

import Codec.QR.Version
import Codec.QR.Mask
import Codec.QR.QR
import Codec.QR.Module
import Data.BitString

import Data.List (maximumBy)
import Data.Bits (xor)

betterScore :: QR -> Version -> Int
betterScore qr v = snd $ maximumBy g [(score (maskModule n v qr t) v, n) 
                                      | n<-[0 .. kindVersionCase 3 7 v]]
   where t = reservedMod v
         g (s,n) (p,m) = compare s p
 
score :: (Module -> Bool) -> Version -> Int
score f = sizeVersionCase (scoreMV f) (scoreV f)

scoreV :: (Module -> Bool) -> Int -> Int
scoreV f s = negate $ 
               sum [sameColorRow f s (i,0) 0 True | i<-[0..s]] + 
               sum [sameColorColumn f s (0,i) 0 True | i<-[0..s]] +
               sum [block f (i,j) | i<-[0..s-1], j<-[i..s-1]] + 
               sum [pattern f (i,j) | i<-[0..s-10], j<-[0..s-10]] + 
               proportion f s

scoreMV :: (Module -> Bool) -> Int -> Int
scoreMV f s = (min sr sl)*16 + (max sr sl)
   where sr = count [f (n,s) | n<-[0..s]]
         sl = count [f (s,n) | n<-[0..s]]
         


sameColorRow :: (Module -> Bool) -> Int -> Module -> Int -> Bool -> Int
sameColorRow f e (x,y) c b | y>e      = s
                           | b'==b     = n (c+1) b'
                           | otherwise = s + n 1 b' 
   where b' = f (x,y)
         s  = if c>=5 then c-2 else 0
         n  = sameColorRow f e (x,y+1)

sameColorColumn :: (Module -> Bool) -> Int -> Module -> Int -> Bool -> Int
sameColorColumn f e (x,y) c b | x>e      = s
                              | b'==b     = n (c+1) b
                              | otherwise = s+ (n 1 b')
   where b' = f (x,y)
         s  = if c>=5 then c-2 else 0
         n  = sameColorColumn f e (x+1,y)

block :: (Module -> Bool) -> Module -> Int
block f (x,y) | and [b1==b2,b2==b3,b3==b4] = 3
              | otherwise                  = 0
   where b1 = f (x  , y  )
         b2 = f (x+1, y  )
         b3 = f (x  , y+1)
         b4 = f (x+1, y+1) 

pattern :: (Module -> Bool) -> Module -> Int
pattern g (x,y) = if r==p1 || r==p2 then 40 else 0 + 
                  if c==p1 || c==p2 then 40 else 0
   where t = True
         f = False
         p1 = [f,f,f,f,t,f,t,t,t,f,t]
         p2 = [t,f,t,t,t,f,t,f,f,f,f]
         r = [g (x,y+i) | i<-[0..10]]
         c = [g (x+i,y) | i<-[0..10]]

proportion :: (Module -> Bool) -> Int -> Int
proportion f s = (10*) $ round $ 20*if prop>0.5 then prop-0.5 else 0.5-prop
   where go (True:xs)  = 1+ go xs
         go (False:xs) = go xs
         go []         = 0
         prop = fromIntegral (go $ [f p | p<- range((0,0),(s,s))]) / 
                fromIntegral ((s+1)^2)