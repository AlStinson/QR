module Codec.QR.Mode 
   (
    module Codec.QR.Mode.Terminator,

    DataSet(..),
    dataAnalisis,
    preEncodeString,
    modeIndicatorLength, 
    minVersion, 
    modeIndicator,
    characterCountLength,
    toBitString
   ) where

import Codec.QR.Core

import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version

import qualified Codec.QR.Mode.Numeric as Numeric
import qualified Codec.QR.Mode.Alphanumeric as Alphanumeric
import qualified Codec.QR.Mode.Kanji as Kanji
import qualified Codec.QR.Mode.Byte as Byte
import Codec.QR.Mode.Terminator

data DataSet = Numeric | Alphanumeric | Kanji | Byte 
   deriving (Eq,Ord)

exclusiveSet :: Char -> DataSet
exclusiveSet c | Numeric.is c = Numeric
               | Alphanumeric.is c = Alphanumeric
               | Kanji.is c = Kanji
               | Byte.is c = Byte

-- Mejorar                                  -- BitCount, CharacterCount
dataAnalisis :: String -> [(DataSet, String)]
dataAnalisis s = [(maximum $ map exclusiveSet s, s)]

                                                    -- BitCount, CharacterCount
preEncodeString :: [(DataSet,String)] -> [(DataSet, BitString, Int,Int)]
preEncodeString = map f
   where f (d,s) = let s' = toBitString d s in (d,s',length s',length s)



modeIndicatorLength :: Version -> Int
modeIndicatorLength = numberVersionCase f g
   where f n = n-1
         g _ = 4

minVersion :: DataSet -> ECLevel -> Version
minVersion x = case x of
  Numeric -> Numeric.minVersion
  Alphanumeric -> Alphanumeric.minVersion
  Kanji -> Kanji.minVersion
  Byte -> Byte.minVersion

modeIndicator :: DataSet -> Version -> BitString
modeIndicator Numeric = Numeric.modeIndicator
modeIndicator Alphanumeric = Alphanumeric.modeIndicator
modeIndicator Byte = Byte.modeIndicator
modeIndicator Kanji = Kanji.modeIndicator

characterCountLength :: DataSet -> Version -> Int
characterCountLength Numeric      = Numeric.characterCountLength 
characterCountLength Alphanumeric = Alphanumeric.characterCountLength
characterCountLength Byte         = Byte.characterCountLength
characterCountLength Kanji        = Kanji.characterCountLength

toBitString :: DataSet -> String -> BitString
toBitString Numeric = Numeric.toBitString
toBitString Alphanumeric = Alphanumeric.toBitString
toBitString Byte = Byte.toBitString
toBitString Kanji = Kanji.toBitString
