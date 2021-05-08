{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Codec.QR.QR where

import Codec.QR.Core
import Codec.QR.Module

type QR = QRArray Module Bool

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
