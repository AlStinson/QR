{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Codec.QR.QR 
   (
    module Codec.QR.QR,
    module Data.Array.QRArray
   ) where

import Codec.QR.Module
import Codec.QR.Version

import Data.Array.QRArray

type QR = QRArray Module Bool

getVersion :: QR -> Maybe Version
getVersion qr = unSize $ (1+) $ getSize qr

getVersionUnsafe :: QR -> Version
getVersionUnsafe qr = unsafeUnSize $ (1+) $ getSize qr

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
