module Codec.QR.String.Mode where

import Codec.QR.Version
import qualified Codec.QR.String.Mode.Numeric as Numeric
import qualified Codec.QR.String.Mode.Alphanumeric as Alphanumeric
import qualified Codec.QR.String.Mode.Byte as Byte
import Data.BitString

data DataSet = Numeric | Alphanumeric | Byte 
   deriving (Eq,Ord,Enum,Show)


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
posibleSetV d = [d .. Byte]

posibleSetMV :: DataSet -> Int -> [DataSet]
posibleSetMV Numeric 1 = [Numeric]
posibleSetMV d 2 = [d .. Alphanumeric]
posibleSetMV x _ = posibleSetV x 
                                                 
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

modeMinVersion :: DataSet -> Version
modeMinVersion x = case x of
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