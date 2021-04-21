module Codec.QR.Module where

import Codec.QR.Version
import Codec.QR.Core

totalModCount :: Version -> Int
totalModCount v = (size v)^2

reservedMod :: Version -> Table Bool
reservedMod = numberVersionCase 
                   reservedModMV 
                   reservedModV

reservedModV :: Int -> Table Bool
reservedModV n = table s s f
   where s = sizeV n
         min = (0,0)
         max = (s-1,s-1)
         f (x,y) = or $ (gur x y):(gdl x y):(x==6):(y==6):(x<=8 && y<=8): 
                        (x>=(s-8) && y<=8):(x<=8 && y>=(s-8)):
                        [x>=i-2 && x<=i+2 && y>=j-2 && y<=j+2 
                        | (i,j) <- alignmentPatternLocationV n]
         gur x y | n<7 = False
                 | otherwise = x>=(s-11) && y<=5 
         gdl x y | n<7 = False
                 | otherwise = x<=6 && y>=(s-11)

reservedModMV :: Int -> Table Bool
reservedModMV n = table s s f
   where s  = sizeMV n
         f (x,y) = or [x==0, y==0, x<=8 && y<=8]

reservedModCount :: Version -> Int
reservedModCount = numberVersionCase  
                       (reservedModCountMV !) 
                       (reservedModCountV !)

reservedModCountMV :: Array Int Int
reservedModCountMV = listArray (1,4) [85, 89, 93, 97] -- MV 1 .. 4

reservedModCountV :: Array Int Int
reservedModCountV = listArray (1, 40) 
   [233 ,266 ,274 ,282 ,290 ,298 ,457 ,465 ,473 ,481 , -- V  1 .. 10
    489 ,497 ,505 ,678 ,686 ,694 ,702 ,710 ,718 ,726 , -- V 11 .. 20
    949 ,957 ,965 ,973 ,981 ,989 ,997 ,1270,1278,1286, -- V 21 .. 30
    1294,1302,1310,1318,1641,1649,1657,1665,1673,1681] -- V 31 .. 40

nonReservedModCount :: Version -> Int
nonReservedModCount v = totalModCount v - reservedModCount v

dataModCount :: Version -> Int
dataModCount v = nonReservedModCount v - (ecCwCount v)*8 - 
                 remainderBitsCount v

dataCwCount :: Version -> Int 
dataCwCount v = div (dataModCount v) 8 
              + if isShortLastWord v then 1 else 0

ecCwCount :: Version -> Int
ecCwCount v = (ecBlocks ! v)*(ecCwPerBlock ! v)

remainderBitsCount :: Version -> Int
remainderBitsCount = versionCase f g
   where f _ = 0
         g v = mod (nonReservedModCount v) 8

remainderBits :: Version -> BitString
remainderBits v = take (remainderBitsCount v) $ repeat False

-- Arreglar para MV
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

--            (isMicro v, y>6, mod y 4, x  , x==s-1)
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

-- Funciones para contruir el QR

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

placeModules :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int
             -> [(Module,Bool)]
placeModules f n m i j = [((a+i,b+j),f a b) | a<-[0..n-1], b<-[0..m-1]]

finderPattern :: Int -> Int -> [(Module,Bool)]
finderPattern = placeModules f 7 7 
   where f x y = or [x==0, x==6, y==0, y==6, 
                     x>=2 && x<=4 && y>=2 && y<=4]

alignmentPatter :: Int -> Int -> [(Module,Bool)]
alignmentPatter = placeModules f 5 5 
   where f x y = or [x==0, x==4, y==0, y==4, x==2 && y==2]



vWhite, hWhite :: Int -> Int -> [(Module,Bool)]
vWhite = placeModules f 7 1
   where f _ _ = False
hWhite = placeModules f 1 8 
   where f _ _ = False

blackAndWhite :: Int -> Int -> Int -> Int -> [(Module,Bool)]
blackAndWhite = placeModules f 
   where f x y = mod (x+y) 2 == 0

reservedArea :: Int -> Int -> Int -> Int -> [(Module,Bool)]
reservedArea = placeModules f
   where f _ _ = False


