{- |
This module can be used to create QR and Micro QR symbols. Only numeric
alphanumeric and byte mode avalieble, so just latin-1 characters can be 
encoded. 

Version and mode are automatic selected, so user dont need to think
about it. It is garantied that the QR recieved will be the most optimiced
one.


It can also be used to decode QR, but with the limitation that the QR must
be in the type defined in this module as QR and not a loaded image. This
can be usefull to check that the symbol was correctly made.
-}

module Codec.QR where

import Codec.QR.String.Mode
import Codec.QR.String.Encode
import Codec.QR.String.Decode
import Codec.QR.ErrorCorrection
import Codec.QR.ErrorCorrection.Encode
import Codec.QR.ErrorCorrection.Decode
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Score
import Codec.QR.Information.Format
import Codec.QR.Information.Version
import Codec.QR.Module
import Codec.QR.Module.Count
import Codec.QR.Module.FunctionPatterns
import Codec.QR.QR
import Codec.QR.Mask
import Codec.QR.Version

import Data.List
import Data.Char
import Data.BitString
import Data.GF256
import Data.Bits (xor)

prueba s = decodeQR . makeQR s

decodeQR :: QR -> Maybe String
decodeQR qr = do 
   v <- getVersion qr
   (e,m) <- getFormatInformation qr v
   bitString <- decodeData e v $ take 
               (nonReservedModCount v - remainderBitsCount v) $ 
               [ xor (qr ! p) $ mask m v p | p<-nonReservedMod v]
   decodeString v $ bitString
 

makeQR :: ECLevel -> String -> QR
makeQR e s = makeQRWith e s Nothing Nothing

makeQRWith :: ECLevel -> String -> Maybe Version -> Maybe Mask -> QR 
makeQRWith e s b c =  (qr //) $ 
                    (zip (versionLocation v) $ (vi++vi)) ++
                    (zip (formatLocation v) $ (fi++fi))
   where minV = minVersion e s $ maybe (MV 1) id b
         costsModes = costsPerVersion s minV
         v = findVersion e minV $ map snd costsModes
         bitString = (++remainderBits v) $ encodeData e v $ encodeString v $ 
                     fst $ costsModes !! (index (minV, V 40) v)
         unMaskQR = array ((0,0),(size v,size v)) $ (blankQR v ++) $
                    zip (nonReservedMod v) $ bitString 
         mask = maybe (betterScore unMaskQR v) id c
         qr = applyMask mask v unMaskQR
         fi = encodeFormat e v mask
         vi = encodeVersion v


minVersion :: ECLevel -> String -> Version -> Version
minVersion e s v = maximum [v, eMinVersion, sMinVersion,cMinVersion]
   where eMinVersion = [MV 1, MV 2, MV 4, V 1] !! fromEnum e
         sMinVersion = modeMinVersion $ maximum $ map exclusiveSet s
         cMinVersion = capacityMinVersion e s 


findVersion :: ECLevel -> Version -> [Int] -> Version
findVersion e m = go $ range (m,V 40) 
   where go [] [] = error "QR codes cant hold that amount of data"
         go (v:vs) (c:cs) | c <= dataModCount e v = v
                          | otherwise = go vs cs

hasCapacity :: ECLevel -> String -> Bool
hasCapacity e = hasCapacityVersion e $ V 40 

hasCapacityVersion :: ECLevel -> Version -> String -> Bool
hasCapacityVersion e v s = dataModCount e v >= (snd $ selectModes s $ v)

url :: ECLevel -> String -> QR
url e s = makeQR e $ "URL:" ++ map toUpper a ++ b
   where (a,b) = splitAt (go $ elemIndices '/' s) s
         go [] = maxBound
         go [x] = x
         go (x:y:xs) | x+1==y = go xs
                     | otherwise = x
