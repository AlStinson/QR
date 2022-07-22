module Codec.QR.Information.Version where

import Codec.QR.Version
import Codec.BHC
import Data.BitString

encodeVersion :: Version -> BitString
encodeVersion = bhcEncode bhcCodeVersion . integralToBitString 6 . number

bhcCodeVersion :: BHCCode
bhcCodeVersion = BHC 18 6 3
