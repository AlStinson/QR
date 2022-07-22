module Test.QR 
   ( iosample
   , generateSamples
   , quickCheck
   ) where

import Codec.QR
import Codec.QR.ErrorCorrection.Level
import Codec.QR.ErrorCorrection
import Codec.QR.Version
import Codec.QR.String.Mode
import Codec.QR.QR
import Codec.QR.Module
import Codec.QR.Module.Count
import Codec.QR.String.Encode
import Codec.ReedSolomon

import Data.Maybe
import Data.Char
import Data.List
import Test.QuickCheck.Gen

arbitraryChars :: DataSet -> [DataSet] -> Int -> Gen (DataSet,String)
arbitraryChars d _  0 = return (d,"")
arbitraryChars d ds n = do m <- frequency [(95,elements [d]),
                                           (5 ,elements ds )]
                           c <- arbitraryChar m
                           nexts <- arbitraryChars m ds (n-1)
                           return $ fmap (c:) nexts

arbitraryChar :: DataSet -> Gen Char
arbitraryChar ds = case ds of
      Numeric -> arbitraryNumeric
      Alphanumeric -> arbitraryAlphanumeric
      Byte -> arbitraryByte

arbitraryNumeric :: Gen Char
arbitraryNumeric = elements ['0' .. '9']

arbitraryAlphanumeric :: Gen Char
arbitraryAlphanumeric = elements $ ['0'..'9']++['A'..'Z']++" $%*+-./:"

arbitraryByte :: Gen Char
arbitraryByte = elements ['\0' .. '\255']

arbitraryString :: ECLevel -> Version -> Gen String
arbitraryString e v = do m <- elements $ dataSets v
                         fstChar <- arbitraryChar m
                         xs <- go m [fstChar]
                         total <- chooseEnum (False,True)
                         n <- chooseInt (1,length xs)
                         elements $ pure $ if total then xs else take n xs
   where dataSets (MV 1) = [Numeric]
         dataSets (MV 2) = [Numeric,Alphanumeric]
         dataSets _      = [Numeric .. Byte]
         capacity = dataModCount e v
         go m xs = let cost = snd $ selectModes xs v
                   in if capacity < cost then return $ init xs
                      else do 
              (m',ys) <- arbitraryChars m (dataSets v) $ max 1 $ flip div 8 $ 
                         capacity - cost - (headerCost v $ maximum $ dataSets v)
              go m' (xs++ys)

arbitraryECLevel :: Version -> Gen ECLevel
arbitraryECLevel v = elements $ case v of
   (MV 1) -> [L]
   (MV 2) -> [L,M]
   (MV 3) -> [L,M]
   (MV 4) -> [L .. Q]
   _      -> [L .. H]

arbitraryVersion :: Gen Version
arbitraryVersion = do n<-chooseInt (0,43)
                      return $ (range (MV 1, V 40)) !! n

arbitraryErrors :: Float -> ECLevel -> Version -> Gen [Module]
arbitraryErrors f e v = do n <- chooseInt (0,max) 
                           go n
   where max = round $ (f*) $ fromIntegral $ sum $ 
               map rsCorrectionCapacity $ rsCodes e v
         go 0 = return []
         go n = do x <- chooseInt (0,size v)
                   y <- chooseInt (0,size v)
                   nexts <- go $ n-1
                   return $ (x,y):nexts

iosample :: Maybe Version -> Float -> IO (Bool,Bool)
iosample v1 f = do 
  v2 <- generate arbitraryVersion
  let v = fromMaybe v2 v1
  e <- generate $ arbitraryECLevel v
  string <- generate $ arbitraryString e v
  err <- generate $ arbitraryErrors f e v
  let qr = makeQRWith e string (Just v) Nothing
      qre = qr // [(p, not $ qr ! p) | p<-err]
      b1 = decodeQR qr == Just string
      b2 = decodeQR qre == Just string
  putStrLn $ "ECLevel: " ++ show e ++ "\nVersion: " ++ show v ++
     "\nErrors: "++ show (length err) ++"\nDecoded correctly without errors: " ++ 
     show b1 ++ "\nDecoded correctly using errors: " ++ show b2
  return (b1,b2)

generateSamples :: Int -> Maybe Version -> Float -> IO()
generateSamples n v f = do {go 1 (0,0); return ()} 
   where go i (a,b) 
          | i>n = do 
             putStrLn $ "Number of Examples: " ++ show n ++ 
                "\nDecoded correctly without errors: " ++ show a ++
                "\nDecoded correctly using errors: " ++ show b ++ "\n"
             return (a,b)
          | otherwise = do
             putStrLn $ "Example " ++ show i
             (a',b') <- iosample v f
             putStrLn "\n"
             go (i+1) (if a' then a+1 else a, if b' then b+1 else b) 

quickCheck :: IO ()
quickCheck = generateSamples 100 Nothing 0.66