module Codec.QR.Module.Count where

import Codec.QR.Version
import Codec.QR.ErrorCorrection.Level
import Data.Array

totalModCount :: Version -> Int
totalModCount v = (1 + size v)^2

reservedModCount :: Array Version Int 
reservedModCount = listArray (MV 1, V 40)
   [85  , 89 , 93 , 97 ,                               -- MV 1 .. 4
    233 ,266 ,274 ,282 ,290 ,298 ,457 ,465 ,473 ,481 , -- V  1 .. 10
    489 ,497 ,505 ,678 ,686 ,694 ,702 ,710 ,718 ,726 , -- V 11 .. 20
    949 ,957 ,965 ,973 ,981 ,989 ,997 ,1270,1278,1286, -- V 21 .. 30
    1294,1302,1310,1318,1641,1649,1657,1665,1673,1681] -- V 31 .. 40

nonReservedModCount :: Version -> Int
nonReservedModCount v = totalModCount v - (reservedModCount ! v) 

nonReservedCwCount :: ECLevel -> Version -> Int
nonReservedCwCount e v = dataCwCount e v + (ecCwCount ! (e,v))

dataModCount :: ECLevel -> Version -> Int
dataModCount e v = nonReservedModCount v - (ecCwCount ! (e,v))*8 -
                   remainderBitsCount v 
                   

dataCwCount :: ECLevel -> Version -> Int 
dataCwCount e v = div (dataModCount e v) 8 + if shortLastword v then 1 else 0

ecCwCount :: Array (ECLevel,Version) Int
ecCwCount = listArray ((L, MV 1),(H,V 40))
-- MV 1, 2, 3, 4, V 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40  EC 
     [2,5,6,8,7,10,15,20,26,36,40,48,60,72,80,96,104,120,132,144,168,180,196,224,224,252,270,300,312,336,360,390,420,450,480,510,540,570,570,600,630,660,720,750,  -- L
      u,6,8,10,10,16,26,36,48,64,72,88,110,130,150,176,198,216,240,280,308,338,364,416,442,476,504,560,588,644,700,728,784,812,868,924,980,1036,1064,1120,1204,1260,1316,1372,  -- M
      u,u,u,14,13,22,36,52,72,96,108,132,160,192,224,260,288,320,360,408,448,504,546,600,644,690,750,810,870,952,1020,1050,1140,1200,1290,1350,1440,1530,1590,1680,1770,1860,1950,2040,  -- Q
      u,u,u,u,17,28,44,64,88,112,130,156,192,224,264,308,352,384,432,480,532,588,650,700,750,816,900,960,1050,1110,1200,1260,1350,1440,1530,1620,1710,1800,1890,1980,2100,2220,2310,2430]  -- H
   where u = undefined

remainderBitsCount :: Version -> Int
remainderBitsCount = versionCase (const 0) f
   where f v = mod (nonReservedModCount v) 8
