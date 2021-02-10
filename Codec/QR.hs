module Codec.QR where

import Data.Matrix
import Test.QuickCheck
import Data.Char

import Data.BitString
import Codec.QR.Mask
import Codec.QR.Version

type QR = Matrix Bool

data DataSet = Numeric | Alphanumeric | Byte | Kanji
data Mode = ECI | Mode DataSet  | StrucAppend | FNC1


-- Las matrices de haskell empiezan en (1,1), no en (0,0), hay que
-- arreglar

finderPattern :: QR
finderPattern = matrix 7 7 f 
    where f (i,j) | i==1 || i==7 || j==1 || j==7 = True
                  | i==2 || i==6 || j==2 || j==6 = False
                  | otherwise = True

zeros :: Int -> Int -> Matrix Bool
zeros n m =  matrix n m (\_ -> False)

blankQR :: Version -> QR
blankQR (V n _) = undefined


applyMask :: Int -> QR -> QR
applyMask n qr = mapPos (\p -> ( \b -> ( b /= (f p)))) qr
   where f | (nrows qr) > 20 = mask n
           | otherwise  = maskMicro n 

type ShowQR = Matrix Char

showqr :: QR -> ShowQR 
showqr qr = matrix (nrows qr) (ncols qr) f
   where f p | qr ! p    = '@'
             | otherwise = ' ' 


------------------------------------------------------------------

numericToBitString :: String -> BitString
numericToBitString [] = []
numericToBitString (x:y:z:xs) = (toBitString (read [x,y,z] :: Int)  10) ++
                           numericToBits xs  
numericToBitString (x:y:_) = toBitString (read [x,y] :: Int)  7 
numericToBitString (x:_) = toBitString (read [x] :: Int)  4 

bitStringToNumeric :: BitString -> String
bitStringToNumeric xs = go 0 [] xs
   where go 10 as bs = (fromBitString as) ++ (go 0 [] bs)
         go n as [] = fromBitString as
         go n as (b:bs) = go (n+1) (as++[b]) bs 

alphanumericToBits :: String -> BitString


alphanumericToInt :: Char -> Int
alphanumericToInt x | elem x ['0' .. '9' ] = 