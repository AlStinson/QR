module Codec.QR.ErrorCorrection.Encode where

import Codec.QR.ErrorCorrection.Level
import Codec.QR.ErrorCorrection
import Codec.QR.Version
import Codec.QR.Module.Count 
import Codec.ReedSolomon
import Data.BitString


encodeData :: ECLevel -> Version -> BitString -> BitString
encodeData e v xs = integralsToBitString w $ 
                    assemble (map snd blocks) (map ecBlock blocks)
   where blocks = blockDivision (rsCodes e v) $ 
                  cw++(padCodewords (c-length cw) v)
         c = dataCwCount e v
         cw = fst $ splitAt c $ bitStringToNums w xs
         w = wordsLength e v
         

blockDivision :: [RSCode] -> [a] -> [(RSCode, [a])]
blockDivision [] [] = []
blockDivision (r:rs) xs = (r,i):(blockDivision rs f)
   where (i,f) = splitAt (rsDataCwCount r) xs


assemble :: [[a]] -> [[a]] -> [a]
assemble xs ys = go [] xs ++ go [] ys
   where go [] [] = []
         go xss [] = go [] $ reverse xss
         go xss ([]:yss) = go xss yss
         go xss ((y:ys):yss) = y:(go (ys:xss) yss)