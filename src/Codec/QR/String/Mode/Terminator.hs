module Codec.QR.String.Mode.Terminator where

import Codec.QR.Version

import Data.BitString

terminator :: Version ->  BitString
terminator v = integralToBitString (terminatorLength v) 0

terminatorLength :: Version -> Int
terminatorLength = numberVersionCase (\v -> 1+2*v) (const 4)