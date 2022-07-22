{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.QR.QR 
   ( module Codec.QR.QR
   , module Data.Array.Base
   , module Data.Ix
   ) where

import Codec.QR.Module
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version
import Data.Array.IArray
import Data.Array.Base 
import Data.Ix

newtype QRArray i e = QR {getArray :: Array i e}

instance IArray QRArray e where
   bounds = bounds . getArray
   numElements = numElements . getArray
   unsafeArray i = QR . unsafeArray i
   unsafeAt = unsafeAt . getArray

type QR = QRArray Module Bool

getVersion :: QR -> Maybe Version
getVersion qr = unSize $ getSize qr

getVersionUnsafe :: QR -> Version
getVersionUnsafe qr = unsafeUnSize $ getSize qr

getSize :: QR -> Int
getSize =  snd . snd . bounds

instance Show QR where
   show qr = let nlines = replicate 4 '\n'
                 spaces = replicate 8 ' ' 
                 s = getSize qr
                 g n m | m == s && n==s = c ++ nlines
                       | m == s         = c ++ ('\n':spaces) ++ g (n+1) 0
                       | otherwise      = c ++ g n (m+1) 
                    where c = if qr!(n,m) then "██" else "  "
             in nlines ++ spaces ++ g 0 0
