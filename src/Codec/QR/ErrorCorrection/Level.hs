module Codec.QR.ErrorCorrection.Level where

import Data.Ix

data ECLevel = L | M | Q | H
   deriving (Eq, Ord, Enum, Show, Ix)
