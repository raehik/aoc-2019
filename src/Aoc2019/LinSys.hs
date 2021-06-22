module Aoc2019.LinSys where

import           Numeric.LinearAlgebra

lsTest1m :: Field t => Matrix t
lsTest1m = (2 >< 2)
  [ 3, -1
  , 2,  4 ]

lsTest1v :: Vector R
lsTest1v = vector [9, 48]

lsTest1Solve :: Vector R
lsTest1Solve = lsTest1m <\> lsTest1v

--lsTest2Diaphantinem :: Field t => Matrix t
--lsTest2Diaphantinem = 
