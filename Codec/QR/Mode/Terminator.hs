module Codec.QR.Mode.Terminator where

import Codec.QR.Core
import Codec.QR.Version

terminator :: Version -> BitString
terminator v = integralToBitString (terminatorLength v) 0

terminatorLength :: Version -> Int
terminatorLength = numberVersionCase (\v -> 1+2*v) (const 4)