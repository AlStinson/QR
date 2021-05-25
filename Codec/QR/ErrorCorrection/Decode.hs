module Codec.QR.ErrorCorrection.Decode where

import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version
import Codec.QR.Module.Count as QR.Count
import Codec.QR.ErrorCorrection
import Codec.ReedSolomon
import Data.BitString
import Data.Array ((!))
import Data.List (transpose)

decodeData :: ECLevel -> Version -> BitString -> Maybe BitString
decodeData e v xs = go [] $ zipWith decode rs $ unAssemble blocks a b
   where (a,b) = splitAt (QR.Count.dataCwCount e v) $ 
                 bitStringToNums (wordsLength e v) xs
         blocks = ecBlocks ! (e,v)
         rs = rsCodes e v
         go xs [] = Just $ integralsToBitString (wordsLength e v) xs
         go xs (Nothing:_) = Nothing
         go xs ((Just ys):zs) = go (xs++ys) zs

unAssemble :: Int -> [a] -> [a] -> [[a]]
unAssemble n xs ys = zipWith (++) 
                  (go (replicate n []) xs (length xs))
                  (go (replicate n []) ys (length ys))
   where go zss zs m | m == 0 = zss
                     | m <  n = let (a,b) = splitAt (n-m) zss
                                in (a++) $ zipWith (++) b $ transpose [zs]
                     | otherwise = let (a,b) = splitAt n zs
                                   in go (zipWith (++) zss $ transpose [a])
                                          b (m-n)