module Codec.QR.Mode.Kanji 
   (
    is,
    modeIndicator,
    characterCountLength,
    toBitString,
    minVersion
   ) where

import Codec.QR.Core

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Data.Char
import Data.BitString

is :: Char -> Bool
is c | o < 0x10000 = kanjiTable ! o
     | otherwise   = False
   where o = ord c 

minVersion :: ECLevel -> Version
minVersion = MV 3

kanjiTable :: Array Int Bool
kanjiTable = array (0,0xffff) $ [ (n, elem n ks) | n<-[0..0xffff]]
   where go a b = map (\(a,b) -> a*0x100+b) $ range (a,b)
         ks = go (0x81,0x40) (0x9f,0x7e) ++
              go (0x81,0x80) (0x9f,0xfc) ++
              go (0xe0,0x40) (0xeb,0x7e) ++
              go (0xe0,0x80) (0xea,0xfc) ++
              go (0xeb,0x40) (0xeb,0xbf)


modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g
   where f 1 = error "Kanji mode not available in MV-1"
         f 2 = error "Kanji mode not available in MV-2"
         f n = integralToBitString 3 (n-1)
         g _ = integralToBitString 8 4

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f 1 = error "Kanji mode not available in MV-1"
         f 2 = error "Kanji mode not available in MV-2"
         f n = n
         g n | n<= 9 =  8
             | n<=26 = 10
             | n<=40 = 12

toBitString :: String -> BitString
toBitString [] = []
toBitString (x:y:xs) = integralToBitString (b1*0xC0+b2) 13 ++toBitString xs 
   where o = (ord x)*0x100 + ord y
         (b1,b2) = divMod (o - if o>0xE040 then 0xC140 else 0x8140) 0x100
         