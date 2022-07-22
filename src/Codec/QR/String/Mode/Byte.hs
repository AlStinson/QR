module Codec.QR.String.Mode.Byte 
  ( is
  , minVersion 
  , modeIndicator
  , characterCountLength 
  , toBitString
  , fromBitString
  , charCost
  ) where 

import Codec.QR.Version
import Data.BitString
import Data.Char (ord, chr)

is :: Char -> Bool
is c = ord c < 256

minVersion :: Version
minVersion = MV 3

modeIndicator :: Version -> BitString
modeIndicator = numberVersionCase f g
   where f 1 = error "Byte mode not available in MV-1"
         f 2 = error "Byte mode not available in MV-2"
         f n = integralToBitString (n-1) 2 
         g _ = integralToBitString 4 4
        

characterCountLength :: Version -> Int
characterCountLength = numberVersionCase f g
   where f 1 = error "Byte mode not available in MV-1"
         f 2 = error "Byte mode not available in MV-2"
         f n = n+1
         g n | n<= 9 =  8
             | n<=40 = 16

toBitString :: String -> BitString
toBitString = integralsToBitString (repeat 8) . map ord


fromBitString :: BitString -> Maybe String
fromBitString xs | mod (length xs) 8 == 0 = Just $ map chr $
                                            bitStringToNums (repeat 8) xs
                 | otherwise              = Nothing


charCost :: Int -> Int
charCost = const 8