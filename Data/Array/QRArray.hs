{-# LANGUAGE 
    FlexibleInstances, 
    MultiParamTypeClasses  
#-}

module Data.Array.QRArray 
   (
    module Data.Array.QRArray,
    module Data.Array.Base,
    module Data.Ix
   ) where

import Data.Array.IArray
import Data.Array.Base
import Data.Ix

newtype QRArray i e = QR {getArray :: Array i e}

instance IArray QRArray e where
   bounds = bounds . getArray
   numElements = numElements . getArray
   unsafeArray i = QR . unsafeArray i
   unsafeAt = unsafeAt . getArray

fArray :: (Ix i, IArray a e, IArray a d) => (i -> e -> d) -> a i e -> a i d
fArray f qr = array b [(i,f i $ qr ! i) | i<-range b]
   where b = bounds qr
