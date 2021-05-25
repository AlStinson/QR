module Codec.QR.Information.Version where

import Codec.QR.Version
import Codec.BHC
import Data.BitString

encodeVersion :: Version -> BitString
encodeVersion = encode bhcCode . integralToBitString 6 . number

bhcCode :: BHCCode
bhcCode = BHC 18 6 3
