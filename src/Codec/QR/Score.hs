module Codec.QR.Score  where

import Codec.QR.Version
import Codec.QR.Mask
import Codec.QR.QR
import Codec.QR.Module
import Data.BitString

import Data.Function (on)
import Data.List (maximumBy,isPrefixOf)
import Data.Bits (xor)

betterScore :: QR -> Version -> Int
betterScore qr v = snd $ maximumBy (compare `on` fst) 
       [(score (maskModule n v qr t) v, n) | n<-[0 .. maxMask v]]
   where t = reservedMod v
 
score :: (Module -> Bool) -> Version -> Int
score f = sizeVersionCase (scoreMV f) (scoreV f)

scoreV :: (Module -> Bool) -> Int -> Int
scoreV f s = negate $ 
               sum [sameColorRow f s (i,0) 0 True | i<-[0..s]] + 
               sum [sameColorColumn f s (0,i) 0 True | i<-[0..s]] +
               sum [block f (i,j) | i<-[0..s-1], j<-[i..s-1]] + 
               sum [pattern [ f (i,j) | j<-[0..s]] | i<-[0..s]] +
               sum [pattern [ f (i,j) | i<-[0..s]] | j<-[0..s]] +
               proportion f s

scoreMV :: (Module -> Bool) -> Int -> Int
scoreMV f s = (min sr sl)*16 + (max sr sl)
   where sr = count [f (n,s) | n<-[0..s]]
         sl = count [f (s,n) | n<-[0..s]]
         

sameColorRow :: (Module -> Bool) -> Int -> Module -> Int -> Bool -> Int
sameColorRow f e (x,y) c b | y>e       = s
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

pattern :: [Bool] -> Int
pattern xs = (if isPrefixOf pattern1 xs then 40 else 0)+
             (if isPrefixOf pattern2 xs then 40 else 0)+ 
             (if null xs then 0 else pattern $ tail xs)

pattern1, pattern2 :: [Bool]
pattern1 = [False,False,False,False,True,False,True,True,True,False,True]
pattern2 = [True,False,True,True,True,False,True,False,False,False,False]

proportion :: (Module -> Bool) -> Int -> Int
proportion f s = (10*) $ round $ (20*) $ abs $ (0.5-) $
  (count $ [f p | p<- range((0,0),(s,s))]) / fromIntegral ((s+1)^2)