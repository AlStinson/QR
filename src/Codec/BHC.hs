module Codec.BHC where

import Data.BitString
import Data.Poly

data BHCCode = BHC { bhcTotalCwCount :: Int,
                     bhcDataCwCount :: Int,
                     bhcCorrectionCapacity :: Int}

bhcEcCwCount :: BHCCode -> Int
bhcEcCwCount rs = bhcTotalCwCount rs - bhcDataCwCount rs

polyGen :: BHCCode -> Poly Bool
polyGen rs = makePoly $ case bhcEcCwCount rs of
   10 -> [1,0,1,0,0,1,1,0,1,1,1]
   12 -> [1,1,1,1,1,0,0,1,0,0,1,0,1]
   _  -> error "BHCCode non defined"

bhcEncode :: BHCCode -> BitString -> BitString
bhcEncode rs xs = (xs ++) $ coefsn e $ 
                  polyMod ((monomial 1 e)*(makePoly xs)) $ polyGen rs
   where e = bhcEcCwCount rs

bhcDecode :: BHCCode -> BitString -> [(BitString,a)] -> Maybe a
bhcDecode rs s [] = Nothing
bhcDecode rs s ((x,y):xs) | (e>=) $ count $ zipWith (+) s x = Just y
                          | otherwise = bhcDecode rs s xs
   where e = bhcCorrectionCapacity rs