module Codec.QR.String.Optimization where

import Codec.QR.Version
import Codec.QR.String.Encode
import Codec.QR.String.Mode
import Data.List (minimumBy, maximumBy)

type ModeSelect = (DataSet,Int)

selectModes :: String -> Version -> (Int,[InterSegment])
selectModes s v = g $ minimumBy f $ foldl go [([],0)] $ map exclusiveSet s 
   where go xs c = [minimumBy f [include v a b | b<-xs] 
                     | a<-posibleSet c v]
         f x y = compare (snd x) (snd y) 
         g (x,y) = (y,applyModes s $ reverse x)
         

applyModes :: String -> [ModeSelect] -> [InterSegment]
applyModes [] [] = []
applyModes xs ((d,n):ys) = (interSegment (d,a)):(applyModes b ys)
   where (a,b) = splitAt n xs


include :: Version -> DataSet -> ([ModeSelect],Int) -> ([ModeSelect],Int)
include v c ([],_) = ([(c,1)], charCost c 1 + headerCost v c)
include v c (ys@((d,m):xs),n) | d == c = ((d,m+1):xs, n + charCost c (m+1))
                              | otherwise = ((c,1):ys, n + charCost c 1
                                                      + headerCost v c)