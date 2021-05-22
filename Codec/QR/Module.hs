module Codec.QR.Module where

import Codec.QR.Version
import Codec.QR.ErrorCorrectionLevel
import Codec.QR.Module.Count

import Data.BitString

import Data.Array
import Data.List ((\\))


type Module = (Int,Int)

reservedMod :: Version -> Array Module Bool
reservedMod v = array b [(p, f p) | p<-range b]
   where b = ((0,0),(s-1,s-1))
         s = size v
         f = numberVersionCase reservedModMV reservedModV v

reservedModV :: Int -> (Int,Int) -> Bool
reservedModV n (x,y) = or $ (gur x y):(gdl x y):(x==6):(y==6):(x<=8 && y<=8): 
                        (x>=(s-8) && y<=8):(x<=8 && y>=(s-8)):
                        [x>=i-2 && x<=i+2 && y>=j-2 && y<=j+2 
                        | (i,j) <- alignmentPatternLocationV n]
   where s = sizeV n
         gur x y | n<7 = False
                 | otherwise = x>=(s-11) && y<=5 
         gdl x y | n<7 = False
                 | otherwise = x<=6 && y>=(s-11)

reservedModMV :: Int -> (Int,Int) -> Bool
reservedModMV _ (x,y) = or [x==0, y==0, x<=8 && y<=8]


remainderBits :: Version -> BitString
remainderBits v = take (remainderBitsCount v) $ repeat False


alignmentPatternLocationV :: Int -> [(Module)]
alignmentPatternLocationV 1 = []
alignmentPatternLocationV n = go3 $ go2 lastPos 
   where npatterns = 2 + div n 7
         fstPos = 6
         s = 17+4*n
         lastPos = s - 7
         sndLastPos = go $ div (6+lastPos*(npatterns-2)+
                     (div (npatterns-1) 2)) (npatterns-1)
         step = lastPos-sndLastPos
         go x = x - if even x then 0 else 1
         go2 x | x<=8       = [6] -- V 32
               | otherwise = x:(go2 $ x-step)
         go3 xs = go4 xs xs \\ [(l,l),(l,f),(f,l)]
            where l = last xs
                  f = head xs
         go4 (x:xs) ys = (zip (repeat x) ys) ++ go4 xs ys
         go4 [] _ = []

versionLocation :: Version -> [(Module)]
versionLocation = numberVersionCase 
                    (const []) 
                    versionLocationV

versionLocationV :: Int -> [(Module)]
versionLocationV n | n<7 = []
                   | otherwise =  [(i,j) | j<-[5,4..0], i<-map (s-) [9,10,11]]
                               ++ [(i,j) | i<-[5,4..0], j<-map (s-) [9,10,11]]
   where s = sizeV n  

formatLocation :: Version -> [(Module)]
formatLocation = numberVersionCase f g
   where f _ = [(8,j) | j<-[1..8]] ++ [(i,8) | i<-[7,6..1]]
         g v = let s = sizeV v 
               in [(i,8) | i<-(map (s-) [1..7])++[8,7,5,4,3,2,1,0]]
               ++ [(8,j) | j<-[0,1,2,3,4,5,7]++ (map (s-) [8,7..1])] 

nonReservedMod :: Version -> [Module]
nonReservedMod v = (s1,s1):(go (s1,s1))
   where t = reservedMod v
         s1 = (size v) - 1
         go (x,y) | (snd n) < 0    = []
                  | t ! n     = go n
                  | otherwise = n:(go n)
            where n = nextModule 
                       (isMicro v, y>6, mod (y+s) 4, x, x==s1) (x,y)
                  s = if isMicro v && odd (number v) then 2 else 0

           -- (isMicro v, y>6, mod y 4, x  , x==s-1)
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
