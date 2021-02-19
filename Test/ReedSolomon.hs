{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.ReedSolomon where

import Codec.ReedSolomon
import Data.Polinomio
import Data.F256

import Test.QuickCheck

data Test = T RSCode (Polinomio F256) (Polinomio F256)
--            Codigo Mensaje          Error
   deriving Show

instance Arbitrary Test where
   arbitrary = do k <- choose (0,128)
                  m'<- choose (0,128)
                  r <- choose (0,div m' 2)

                  e <- choose (0,r)
                  pos <- vectorOf e $ choose (0,m'+k)
                  errores <- vectorOf e arbitrary


                  coe <- vectorOf m' arbitrary
                  
                  return $ T (RS (m'+k) k r) 
                             (creaPolinomio coe)
                             (deListas errores pos)
                  

instance Arbitrary F256 where
   arbitrary = do n <- choose (0,255)
                  return $ fromInt n 

prop :: Test -> Bool
prop (T rs p q) = corrigeErrores rs (cifrado+q) == cifrado
   where cifrado = codifica rs p

-- *Test.ReedSolomon> quickCheck(withMaxSuccess 10 prop)
-- +++ OK, passed 10 tests.
-- (127.11 secs, 58,219,138,424 bytes)