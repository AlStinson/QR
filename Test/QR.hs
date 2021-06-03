{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}

module Test.QR where

import Codec.QR
import Codec.QR.ErrorCorrection.Level
import Codec.QR.Version
import Codec.QR.String.Mode
import Codec.QR.QR
import Codec.QR.Module
import Codec.QR.String.Encode

import Data.Char
import Test.QuickCheck

data ArbitraryQR = A QR ECLevel String

newtype CharString = CS String

instance Arbitrary CharString where
   arbitrary = do
      n <- Test.QuickCheck.getSize
      chars <- infiniteListOf $ genSegment (choose (0,n)) arbitrary 
      return $ CS $ take n $ concat chars
         where genSegment n d = do
                 n' <- n
                 d' <- d
                 vectorOf n' $ elements $ case d' of
                  Numeric -> ['0' .. '9']
                  Alphanumeric -> ['0'..'9']++['A'..'Z']++" $%*+-./:"
                  Byte -> ['\0' .. '\255']

instance Show ArbitraryQR
   where show (A qr e xs) = show e++"\n"++show xs

instance Arbitrary ECLevel where
   arbitrary = chooseEnum (L,H)

instance Arbitrary DataSet where
   arbitrary = chooseEnum (Numeric,Byte)

instance Arbitrary ArbitraryQR where
   arbitrary = do
      e <- arbitrary
      n' <- Test.QuickCheck.getSize
      let n = 1 + div (n'*capacityMinVersionTable ! (e,V 40)) 100 
      (CS string) <- resize n arbitrary
      let qr = makeQR e string
          s = Codec.QR.QR.getSize qr
          r = do a <- choose (0,s)
                 b <- choose (0,s)
                 return (a,b)
      ne <- choose (0,((s+1)^2 *) $ round $ (!!) 
            [0.07,0.15,0.25,0.30] $ fromEnum e)
      errors <- vectorOf ne r
      let errorQR = qr // [(p,not (qr ! p)) | p<-errors]
      return $ A errorQR e string


proofTest = quickCheckWith stdArgs{maxSuccess=100} test

test :: ArbitraryQR -> Property
test (A qr e xs) = hasCapacity e xs ==> 
                   collect (getVersionUnsafe qr) $ 
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