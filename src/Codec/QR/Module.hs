module Codec.QR.Module where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Module.Count
import Data.BitString
import Data.Array
import Data.List ((\\))


type Module = (Int,Int)

reservedMod :: Version -> Array Module Bool
reservedMod v = array b [(p, f p) | p<-range b]
   where b = ((0,0),(size v,size v))
         f = numberVersionCase reservedModMV reservedModV v

reservedModV :: Int -> Module -> Bool
reservedModV n (x,y) = or $ (gur x y):(gdl x y):(x==6):(y==6):(x<=8 && y<=8): 
                        (x>=(s-7) && y<=8):(x<=8 && y>=(s-7)):
                        [x>=i-2 && x<=i+2 && y>=j-2 && y<=j+2 
                        | (i,j) <- alignmentPatternLocationV n]
   where s = sizeV n
         gur x y | n<7 = False
                 | otherwise = x>=(s-10) && y<=5 
         gdl x y | n<7 = False
                 | otherwise = x<=6 && y>=(s-10)

reservedModMV :: Int -> Module -> Bool
reservedModMV _ (x,y) = or [x==0, y==0, x<=8 && y<=8]


remainderBits :: Version -> BitString
remainderBits v = take (remainderBitsCount v) $ repeat False

alignmentPatternLocationV :: Int -> [(Module)]
alignmentPatternLocationV 1 = []
alignmentPatternLocationV n = go $ list lastPos
   where npatterns = div n 7
         npatterns1 = npatterns + 1
         fstPos = 6
         lastPos = sizeV n - 6
         sndLastPos = roundEven $ div (fstPos + 
                      lastPos*npatterns + div npatterns1 2)
                      npatterns1
         step = lastPos-sndLastPos
         roundEven x | even x = x
                     | otherwise = x-1
         list x | x<=8 = [6]
                | otherwise = x:(list $ x-step)
         go xs = let l = last xs
                     f = head xs
                 in (foldr (\x ys -> zip (repeat x) xs ++ ys) 
                     [] xs) \\ [(l,l),(l,f),(f,l)]  


versionLocation :: Version -> [(Module)]
versionLocation = numberVersionCase (const []) versionLocationV

versionLocationV :: Int -> [(Module)]
versionLocationV n | n<7 = []
                   | otherwise =  [(i,j) | j<-[5,4..0], i<-map (s-) [8,9,10]]
                               ++ [(i,j) | i<-[5,4..0], j<-map (s-) [8,9,10]]
   where s = sizeV n  

formatLocation :: Version -> [(Module)]
formatLocation = numberVersionCase f g
   where f _ = [(8,j) | j<-[1..8]] ++ [(i,8) | i<-[7,6..1]]
         g v = let s = sizeV v 
               in [(i,8) | i<-(map (s-) [0..6])++[8,7,5,4,3,2,1,0]]
               ++ [(8,j) | j<-[0,1,2,3,4,5,7]++ (map (s-) [7,6..0])] 

nonReservedMod :: Version -> [Module]
nonReservedMod v = (s,s):(go (s,s))
   where t = reservedMod v
         s = size v
         go (x,y) | (snd n) < 0    = []
                  | t ! n     = go n
                  | otherwise = n:(go n)
            where n = nextModule 
                       (isMicro v, y>6, mod (y+k) 4, x, x==s) (x,y)
                  k = if isMicro v && odd (number v) then 2 else 0

           -- (isMicro v, y>6 , mod y 4, x  , x==s-1)
nextModule :: (Bool     , Bool, Int    , Int, Bool  ) -> Module -> Module

nextModule (_, True , 0, _, _    ) (x,y) = (x  , y-1)
nextModule (_, True , 3, 0, _    ) (x,y) = (x  , y-1)
nextModule (_, True , 3, _, _    ) (x,y) = (x-1, y+1)
nextModule (_, True , 2, _, _    ) (x,y) = (x  , y-1)
nextModule (_, True , 1, _, True ) (x,y) = (x  , y-1)
nextModule (_, True , 1, _, False) (x,y) = (x+1, y+1)

nextModule (False, False, _, a, b) (x,y) = 
            nextModule (False, True, mod (y+1) 4, a, b) (x,y)
nextModule (True , False, a, b, c) (x,y) = 
            nextModule (False , True, a, b, c) (x,y)
