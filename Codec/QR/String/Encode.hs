module Codec.QR.String.Encode where

import Codec.QR.Version
import Codec.QR.String.Mode
import Codec.QR.String.Mode.Terminator
import Data.BitString


type InterSegment = (DataSet,BitString,Int,Int)
                              --BitCount, Character Count


costs :: (Version -> (Int,[InterSegment])) ->
         [(Int,[InterSegment])]
costs f = [f v | v<-micros] ++ replicate 9  (f $ V 1) ++
      replicate 17 (f $ V 10) ++ replicate 14 (f $ V 27)


interSegment :: (DataSet,String) -> InterSegment
interSegment (d,s) = (d,s',length s',length s)
   where s' = toBitString d s 

preEncodeString :: [(DataSet,String)] -> Version -> (Int,[InterSegment])
preEncodeString xs v = (foldl f 0 pre, pre)
   where pre = map interSegment xs
         f n (a,_,b,_) = modeIndicatorLength v + 
                       characterCountLength a v + 
                       b + n

encodeString :: Version -> [InterSegment] -> BitString
encodeString v = foldr f $ terminator v 
   where f (x,y,z,w) bs = modeIndicator x v ++ (integralToBitString 
                        (characterCountLength x v) w) ++ y ++ bs
