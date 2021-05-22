{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}

module Test.QR where

import Codec.QR
import Codec.QR.ErrorCorrectionLevel
import Codec.QR.Version
import Codec.QR.Mode
import Codec.QR.QR
import Codec.QR.Module

import Data.Char
import Test.QuickCheck

data ArbitraryQR = A QR ECLevel String

instance Show ArbitraryQR
   where show (A qr e xs) = show e++"\n"++show xs


chooseEnum :: Enum a => (a, a) -> Gen a
chooseEnum (a,b) = do n<-choose (fromEnum a, fromEnum b)
                      return $ toEnum n 

instance Arbitrary ECLevel where
   arbitrary = chooseEnum (L,H)

instance Arbitrary DataSet where
   arbitrary = chooseEnum (Numeric,Byte)

instance Arbitrary ArbitraryQR where
   arbitrary = do
      s <- Test.QuickCheck.getSize
      e <- arbitrary
      let n = max 1 $ min s $ [1817,1435,1024,784] !! (fromEnum e)
      intervals <- infiniteListOf $ choose (0,n)
      string <- generateIntervals $ takeIntervals n intervals
      let qr = makeQR string e
          s = Codec.QR.QR.getSize qr
      ex <- choose (0,s)
      ey <- choose (0,s)
      ne <- choose (0,((s+1)^2 *) $ round $ (!!) [0.07,0.15,0.25,0.30] $ fromEnum e)
      errors <- vectorOf ne $ elements [(ex,ey)]
      let errorQR = qr // [(p,not (qr ! p)) | p<-errors]
      return $ A errorQR e string
         where takeIntervals n (x:xs) = if (x>=n) then [n] 
                                        else x:(takeIntervals (n-x) xs)
               genChar d = elements $ case d of
                  Numeric -> ['0' .. '9']
                  Alphanumeric -> ['0'..'9']++['A'..'Z']++" $%*+-./:"
                  Byte -> ['\0' .. '\255']
               generateIntervals [] = return []
               generateIntervals (x:xs) = do 
                     set <- arbitrary
                     this <- vectorOf x $ genChar set
                     other <- generateIntervals xs
                     return $ this ++ other 
{-
 add funcion cabe en qr
 max size
-}

proofTest = quickCheckWith stdArgs{maxSize = 1817} test

test :: ArbitraryQR -> Property
test (A qr e xs) = collect (getVersionUnsafe qr) $ 
                   (Just xs) == (decodeQR qr) 
{-
+++ OK, passed 100 tests:
10% V  19
 7% V  23
 6% V  22
 6% V  25
 6% V  31
 5% V  15
 5% V  17
 5% V  27
 4% V  10
 4% V  14
 4% V  20
 4% V  24
 3% V  12
 3% V  21
 3% V  26
 3% V  28
 3% V  6
 2% V  11
 2% V  13
 2% V  16
 2% V  18
 2% V  29
 2% V  4
 2% V  8
 1% MV 2
 1% V  2
 1% V  3
 1% V  30
 1% V  5
(654.59 secs, 353,714,558,224 bytes)
-}