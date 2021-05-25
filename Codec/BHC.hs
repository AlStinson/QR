module Codec.BHC where

import Data.BitString
import Data.Poly

data BHCCode = BHC 
               {
                totalCwCount :: Int,
                dataCwCount :: Int,
                correctionCapacity :: Int
               }

ecCwCount :: BHCCode -> Int
ecCwCount rs = totalCwCount rs - dataCwCount rs

polyGen :: BHCCode -> Poly Bool
polyGen rs = makePoly $ case ecCwCount rs of
   10 -> [1,0,1,0,0,1,1,0,1,1,1]
   12 -> [1,1,1,1,1,0,0,1,0,0,1,0,1]
   _  -> error "BHCCode non defined"

encode :: BHCCode -> BitString -> BitString
encode rs xs = (xs ++) $ drop 1 $ coefs $ (p+) $
                polyMod (p*(makePoly xs)) $ polyGen rs
   where p = monomial 1 $ ecCwCount rs

decode :: BHCCode -> [Bool] -> [([Bool],a)] -> Maybe a
decode rs s [] = Nothing
decode rs s ((x,y):xs) | (e>=) $ count $ zipWith (+) s x = Just y
                       | otherwise = decode rs s xs
   where e = correctionCapacity rs