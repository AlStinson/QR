module Codec.QR 
   ( ECLevel (..)
   , Version (..)
   , Module
   , Mask
   , makeQR
   , makeJustQR
   , fastQR
   , url
   , email
   , makeQRWith
   , decodeQR
   , minVersion
   , hasCapacity
   , Extension (..)
   , saveImage
   ) where

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
import Codec.QR.Image

import Data.List
import Data.Char
import Data.BitString
import Data.Bits (xor)
import Data.Maybe

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

makeJustQR :: ECLevel -> String -> QR
makeJustQR e s = makeQRWith e s (Just $ V 1) Nothing

fastQR :: ECLevel -> String -> QR
fastQR e s = makeQRWith e s Nothing (Just 0)

makeQRWith :: ECLevel -> String -> Maybe Version -> Maybe Mask -> QR 
makeQRWith e s b c =  (qr //) $ 
                    (zip (versionLocation v) $ (vi++vi)) ++
                    (zip (formatLocation v) $ (fi++fi))
   where minV = max (capacityMinVersion e s) (fromMaybe (MV 1) b)
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


findVersion :: ECLevel -> Version -> [Int] -> Version
findVersion e m = go $ range (m,V 40) 
   where go [] [] = error "QR codes cant hold that amount of data"
         go (v:vs) (c:cs) | c <= dataModCount e v = v
                          | otherwise = go vs cs

minVersion :: ECLevel -> String -> Version
minVersion e s = findVersion e minV $ map snd $ costsPerVersion s minV
   where minV = capacityMinVersion e s

hasCapacity :: ECLevel -> String -> Bool
hasCapacity e s = dataModCount e (V 40) >= (snd $ selectModes s $ (V 40))

url :: ECLevel -> String -> QR
url e s = makeQR e $ scheme ++ map toUpper a ++ b
   where (a,b) = splitAt (fromMaybe maxBound $ elemIndex '/' web) web
         d1 = stripPrefix "http://" s
         d2 = stripPrefix "https://" s
         (scheme,web) = if isJust d1 then ("HTTP://",fromJust d1) else  
                        ("HTTPS://", fromMaybe s d2)

email :: ECLevel -> String -> QR
email e s = makeQR e $ "mailto:" ++ a ++ map toUpper b
   where (a,b) = splitAt (fromMaybe maxBound $ elemIndex '@' s) s
                                                

