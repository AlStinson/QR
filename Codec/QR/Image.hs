module Codec.QR.Image where

import Codec.QR.Core
import Codec.QR.QR
import Codec.Picture


-- 

createImage :: FilePath -> Int -> Int -> QR -> IO()
createImage path scale border qr = savePngImage path $ 
                                   ImageY8 $ generateImage f p p
   where s = getSize qr
         p = (scale*) $ s + 2*border + 1
         f x y | and [x'>=0,y'>=0,x'<=s,y'<=s] = if qr ! (y',x') then 0 else 255
               | otherwise = 255
            where x' = (div x scale) - border
                  y' = (div y scale) - border