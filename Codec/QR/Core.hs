module Codec.QR.Core
   ( 
     module M,
   ) where

import Data.BitString as M
import Data.List as M ((\\), maximumBy, minimumBy)
import Data.Char as M (ord)
import Data.Bits as M (xor)
import Data.Array.IArray as M
import Data.Table as M 
import Data.Array.QRArray as M
import Data.GF256 as M (GF256)

import Data.Bool.Instances