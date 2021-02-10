module Codec.QR.Version where

import Codec.QR.ErrorCorrection

data Version = V Int ErrorCorrection | MV Int ErrorCorrection 

size :: Version -> Int
size (V a _) = 17+4*a
size (MV a _) = 9+2*a

validVersion :: Version -> Bool
validVersion (V a _) = a>0 && a<41
validVersion (MV a _) = a>0 && a<5

modeIndicatorLength :: Version -> Int
modeIndicatorLength (MV n _) = n-1
modeIndicatorLength (V _ _)  = 4