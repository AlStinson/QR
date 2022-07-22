module Codec.QR.String.Decode (decodeString) where

import Codec.QR.String.Mode
import Codec.QR.String.Mode.Terminator
import Codec.QR.Version
import Data.BitString
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe

decodeString :: Version -> BitString -> Maybe String
decodeString v xs = do modes <- unSelectModes v xs
                       let list = map (\(x,y) -> fromBitString x y) modes
                       if any isNothing list then Nothing 
                       else Just $ concat $ catMaybes list 

unSelectModes :: Version -> BitString -> Maybe [(DataSet,BitString)]
unSelectModes v xs = go [] xs
   where go ys xs | isPrefixOf (terminator v) xs = Just $ reverse ys
                  | all not xs = Just $ reverse ys
                  | null mode = Nothing
                  | otherwise = let (a,b) = splitBitString v zs d
                                in go ((d,a):ys) b
            where mode = detectMode v xs
                  (d,zs) = head mode

detectMode :: Version -> BitString -> [(DataSet,BitString)]
detectMode v xs = catMaybes $ fmap 
   (\x -> fmap (\y -> (x,y)) $ stripPrefix (modeIndicator x v) xs)
   [Numeric,Alphanumeric,Byte]


splitBitString :: Version -> BitString -> DataSet ->
                 (BitString, BitString)
splitBitString v xs d = flip splitAt b $  
                        sum [charCost d i | i<-[1 .. bitStringToNum a]]
   where (a,b) = splitAt (characterCountLength d v) xs