module Intcode.Programs where

d2TestProgramLong :: [Int]
d2TestProgramLong =
  [ 1,9,10,3
  , 2,3,11,0
  , 99
  , 30,40,50
  ]

d2TestProgramLongOut :: [Int]
d2TestProgramLongOut =
  [ 3500,9,10,70
  , 2,3,11,0
  , 99
  , 30,40,50
  ]

d2TestProgramShortAdd, d2TestProgramShortAddOut :: [Int]
d2TestProgramShortAdd    = [ 1,0,0,0,99 ]
d2TestProgramShortAddOut = [ 2,0,0,0,99 ]
