module Data.Table 
   ( 
     Table,
     table, 
     listTable,
     mapTable,
     update

   ) where

import Data.Array

type Table a = Array (Int,Int) a 

table :: Int -> Int -> ((Int,Int) -> a) -> Table a
table i j f = array (findex,lindex) [(p, f p) | p<-range(findex,lindex)]
   where findex = (0,0)
         lindex = (i-1,j-1)

listTable :: Int -> Int -> [((Int,Int),a)] -> Table a
listTable i j = array ((0,0),(i-1,j-1)) 

update :: Table a -> [((Int,Int),a)] -> Table a
update t u = t // u

changeIndexes :: Table a -> (Int,Int) -> Table a
changeIndexes t (i,j) = ixmap ((a+i,b+j),(c+i,d+j)) (\(x,y) -> (x+i,y+j)) t
   where ((a,b),(c,d)) = bounds t

include :: Table a -> (Int,Int,Table a) -> Table a
include t (i,j,t') = update t $ assocs $ changeIndexes t' (i,j)

mapTable :: ((Int,Int) -> a -> b) -> Table a -> Table b
mapTable f t = table (i+1) (j+1) g
   where (_,(i,j)) = bounds t
         g p = f p $ t ! p
 