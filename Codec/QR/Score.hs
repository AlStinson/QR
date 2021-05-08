module Codec.QR.Score (betterScore,score) where

import Codec.QR.Core
import Codec.QR.Version
import Codec.QR.Mask
import Codec.QR.QR
import Codec.QR.Module

betterScore :: Version -> QR -> (QR,Int)
betterScore v qr = g $ maximumBy f 
                   [(q,n,score q v) | n<-[0..max], q<-[applyMask n v qr]]
   where f (_,_,s1) (_,_,s2) = compare s1 s2
         g (x,y,z) = (x,y)
         max = kindVersionCase 3 7 v

score :: QR -> Version -> Int
score qr = kindVersionCase (scoreMV qr) (scoreV qr)

scoreV :: QR -> Int
scoreV t = negate $ 
               sum [sameColorRow t s (i,0) 0 True | i<-[0..s]] + 
               sum [sameColorColumn t s (0,i) 0 True | i<-[0..s]] +
               sum [block t (i,j) | i<-[0..s-1], j<-[i..s-1]] + 
               sum [pattern t (i,j) | i<-[0..s-10], j<-[0..s-10]] + 
               proportion t s
   where s = getSize t

scoreMV :: QR -> Int
scoreMV t = (min sr sl)*16 + (max sr sl)
   where s = getSize t
         sr = sum [if t ! (n,s) then 1 else 0 | n<-[0..s]]
         sl = sum [if t ! (s,n) then 1 else 0 | n<-[0..s]]
         


sameColorRow :: QR -> Int -> Module -> Int -> Bool -> Int
sameColorRow t f (x,y) c b | y>f      = s
                           | b'==b     = n (c+1) b'
                           | otherwise = s + n 1 b' 
   where b' = t!(x,y)
         s  = if c>=5 then c-2 else 0
         n  = sameColorRow t f (x,y+1)

sameColorColumn :: QR -> Int -> Module -> Int -> Bool -> Int
sameColorColumn t f (x,y) c b | x>f      = s
                              | b'==b     = n (c+1) b
                              | otherwise = s+ (n 1 b')
   where b' = t!(x,y)
         s  = if c>=5 then c-2 else 0
         n  = sameColorColumn t f (x+1,y)

block :: QR -> Module -> Int
block t (x,y) | and [b1==b2,b2==b3,b3==b4] = 3
              | otherwise                  = 0
   where b1 = t ! (x  , y  )
         b2 = t ! (x+1, y  )
         b3 = t ! (x  , y+1)
         b4 = t ! (x+1, y+1) 

pattern :: QR -> Module -> Int
pattern tb (x,y) = if r==p1 || r==p2 then 40 else 0 + 
                   if c==p1 || c==p2 then 40 else 0
   where t = True
         f = False
         p1 = [f,f,f,f,t,f,t,t,t,f,t]
         p2 = [t,f,t,t,t,f,t,f,f,f,f]
         r = [tb ! (x,y+i) | i<-[0..10]]
         c = [tb ! (x+i,y) | i<-[0..10]]

proportion :: QR -> Int -> Int
proportion t s = (10*) $ round $ 20*if prop>0.5 then prop-0.5 else 0.5-prop
   where go (True:xs)  = 1+ go xs
         go (False:xs) = go xs
         go []         = 0
         prop = fromIntegral (go $ elems t) / fromIntegral (s^2)