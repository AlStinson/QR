module Codec.QR.Core 
   ( 
     module M,

     Mask,
     Module,
     QR,

     u

   ) where

import Data.BitString as M
import Data.List as M ((\\), maximumBy)
import Data.Table as M
import Data.Char as M (ord)
import Data.Bits as M (xor)

import Data.Bool.Instances


type Mask = Int
type Module = (Int,Int)
type QR = Table Bool


-- Shortening for undefined

u :: a
u = undefined