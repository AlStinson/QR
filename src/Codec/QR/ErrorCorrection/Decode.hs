module Codec.QR.ErrorCorrection.Decode where

import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version
import Codec.QR.Module.Count
import Codec.QR.ErrorCorrection
import Codec.ReedSolomon
import Data.BitString
import Data.Array ((!))
import Data.List (transpose)
import Control.Monad

decodeData :: ECLevel -> Version -> BitString -> Maybe BitString
decodeData e v xs = fmap (integralsToBitString words) $ foldl1 (liftM2 (++)) $ 
            zipWith rsDecode (rsCodes e v) $ unAssemble (ecBlocks ! (e,v)) a b
   where (a,b) = splitAt (dataCwCount e v) $ bitStringToNums words xs
         words = wordsLength e v

unAssemble :: Int -> [a] -> [a] -> [[a]]
unAssemble n xs ys = zipWith (++) (unAssemble' xs) (unAssemble' ys)
   where unAssemble' zs = go (replicate n []) zs (length zs) 
         go zss zs m 
          | m == 0 = zss
          | m <  n = let (a,b) = splitAt (n-m) zss
                     in (a++) $ zipWith (++) b $ transpose [zs]
          | otherwise = let (a,b) = splitAt n zs
                        in go (zipWith (++) zss $ transpose [a]) b (m-n)