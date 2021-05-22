module Codec.QR.Mode where

import Codec.QR.Version
import qualified Codec.QR.Mode.Numeric as Numeric
import qualified Codec.QR.Mode.Alphanumeric as Alphanumeric
import qualified Codec.QR.Mode.Byte as Byte
import Codec.QR.Mode.Terminator

import Data.BitString

import Data.Maybe
import Data.List

data DataSet = Numeric | Alphanumeric | Byte 
   deriving (Eq,Ord,Enum,Show)

type ModeSelect = (DataSet,Int)
type InterSegment = (DataSet,BitString,Int,Int)
                                   -- BitCount, CharacterCount

selectModes :: Version -> String -> (Int,[InterSegment])
selectModes v s = g $ minimumBy f $ foldl go [([],0)] $ map exclusiveSet s 
   where go xs c = [minimumBy f [include v a b | b<-xs] 
                     | a<-posibleSet c v]
         f x y = compare (snd x) (snd y) 
         g (x,y) = (y,applyModes s $ reverse x)
         

include :: Version -> DataSet -> ([ModeSelect],Int) -> ([ModeSelect],Int)
include v c ([],_) = ([(c,1)], charCost c 1 + headerCost v c)
include v c (ys@((d,m):xs),n) | d == c = ((d,m+1):xs, n + charCost c (m+1))
                              | otherwise = ((c,1):ys, n + charCost c 1
                                                      + headerCost v c)


applyModes :: String -> [ModeSelect] -> [InterSegment]
applyModes [] [] = []
applyModes xs ((d,n):ys) = let (a,b) = splitAt n xs
                               s = toBitString d a
                           in (d,s,length s,n):(applyModes b ys)


exclusiveSet :: Char -> DataSet
exclusiveSet c | Numeric.is c = Numeric
               | Alphanumeric.is c = Alphanumeric
               | Byte.is c = Byte
               | otherwise = error $ "Non valid character: "++show c

posibleSet :: DataSet -> Version -> [DataSet]
posibleSet d = numberVersionCase 
               (posibleSetMV d)
               (const $ posibleSetV d)

posibleSetV :: DataSet -> [DataSet]
posibleSetV Numeric = [Numeric, Alphanumeric,Byte]
posibleSetV Alphanumeric = [Alphanumeric,Byte]
posibleSetV Byte = [Byte]

posibleSetMV :: DataSet -> Int -> [DataSet]
posibleSetMV Numeric 1 = [Numeric]
posibleSetMV Numeric 2 = [Numeric,Alphanumeric]
posibleSetMV Alphanumeric 2 = [Alphanumeric]
posibleSetMV x _ = posibleSetV x 

                                                    
preEncodeString :: [(DataSet,String)] -> [InterSegment]
preEncodeString = map f
   where f (d,s) = let s' = toBitString d s in (d,s',length s',length s)

encodeString :: Version -> [InterSegment] -> BitString
encodeString v = foldr f $ terminator v 
   where f (x,y,z,w) bs = modeIndicator x v ++ (integralToBitString 
                        (characterCountLength x v) w) ++ y ++ bs

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

costs :: (Version -> (Int,[InterSegment])) ->
         [(Int,[InterSegment])]
costs f = [f v | v<-micros] ++ replicate 9  (f $ V 1) ++
      replicate 17 (f $ V 10) ++ replicate 14 (f $ V 27)
 
bitLength :: Version -> [InterSegment] -> Int
bitLength v = foldl f 0
   where f n (a,_,b,_) = modeIndicatorLength v + 
                       characterCountLength a v + 
                       b + n

modeIndicatorLength :: Version -> Int
modeIndicatorLength = numberVersionCase f g
   where f n = n-1
         g _ = 4

headerCost :: Version -> DataSet -> Int
headerCost v d = modeIndicatorLength v + characterCountLength d v

charCost :: DataSet -> Int -> Int
charCost x = case x of
   Numeric -> Numeric.charCost
   Alphanumeric -> Alphanumeric.charCost
   Byte -> Byte.charCost

minVersion :: DataSet -> Version
minVersion x = case x of
  Numeric -> Numeric.minVersion
  Alphanumeric -> Alphanumeric.minVersion
  Byte -> Byte.minVersion

modeIndicator :: DataSet -> Version -> BitString
modeIndicator Numeric = Numeric.modeIndicator
modeIndicator Alphanumeric = Alphanumeric.modeIndicator
modeIndicator Byte = Byte.modeIndicator

characterCountLength :: DataSet -> Version -> Int
characterCountLength Numeric      = Numeric.characterCountLength 
characterCountLength Alphanumeric = Alphanumeric.characterCountLength
characterCountLength Byte         = Byte.characterCountLength

toBitString :: DataSet -> String -> BitString
toBitString Numeric = Numeric.toBitString
toBitString Alphanumeric = Alphanumeric.toBitString
toBitString Byte = Byte.toBitString

fromBitString :: DataSet -> BitString -> Maybe String
fromBitString Numeric = Numeric.fromBitString
fromBitString Alphanumeric = Alphanumeric.fromBitString
fromBitString Byte = Byte.fromBitString