{-# LANGUAGE TypeFamilies #-}

module Aoc2019.Intcode.Programs.D2 where

import           Prelude hiding (read)
import           Aoc2019.Intcode.Interpreter.Symbolic
import qualified Data.Map.Lazy as Map

{-

i :: a -> Sym a
i = SymConst

v :: Var -> Sym a
v x = SymExp $ Map.fromList [(Map.fromList [(x, 1)], 1)]

-- | My day 2 program pre-parsed. TODO shouldn't be Sym, but I don't have orig
--   lol.
--
-- My solution was noun=82 (->79), verb=26 (->23).
d2p2Solved :: [Sym Int]
d2p2Solved =
  [ i 1, i 82, i 26, i 3
  , i 1, i 1, i 2, i 3
  , i 1, i 3, i 4, i 3
  , i 1, i 5, i 0, i 3
  , i 2, i 10, i 1, i 19
  , i 2, i 9, i 19, i 23
  , i 2, i 13, i 23, i 27
  , i 1, i 6, i 27, i 31
  , i 2, i 6, i 31, i 35
  , i 2, i 13, i 35, i 39
  , i 1, i 39, i 10, i 43
  , i 2, i 43, i 13, i 47
  , i 1, i 9, i 47, i 51
  , i 1, i 51, i 13, i 55
  , i 1, i 55, i 13, i 59
  , i 2, i 59, i 13, i 63
  , i 1, i 63, i 6, i 67
  , i 2, i 6, i 67, i 71
  , i 1, i 5, i 71, i 75
  , i 2, i 6, i 75, i 79
  , i 1, i 5, i 79, i 83
  , i 2, i 83, i 6, i 87
  , i 1, i 5, i 87, i 91
  , i 1, i 6, i 91, i 95
  , i 2, i 95, i 6, i 99
  , i 1, i 5, i 99, i 103
  , i 1, i 6, i 103, i 107
  , i 1, i 107, i 2, i 111
  , i 1, i 111, i 5, i 0
  , i 99
  , i 2, i 14, i 0 , i 0
  ]

-- | My day 2 program annotated with noun and verb variables.
--
-- My solution was noun=82 (->75), verb=26 (->19).
d2Symbolic :: [Sym Int]
d2Symbolic =
  [ i 1, v "noun", v "verb", i 3
  , i 1, i 1, i 2, i 3
  , i 1, i 3, i 4, i 3
  , i 1, i 5, i 0, i 3
  , i 2, i 10, i 1, i 19
  , i 2, i 9, i 19, i 23
  , i 2, i 13, i 23, i 27
  , i 1, i 6, i 27, i 31
  , i 2, i 6, i 31, i 35
  , i 2, i 13, i 35, i 39
  , i 1, i 39, i 10, i 43
  , i 2, i 43, i 13, i 47
  , i 1, i 9, i 47, i 51
  , i 1, i 51, i 13, i 55
  , i 1, i 55, i 13, i 59
  , i 2, i 59, i 13, i 63
  , i 1, i 63, i 6, i 67
  , i 2, i 6, i 67, i 71
  , i 1, i 5, i 71, i 75
  , i 2, i 6, i 75, i 79
  , i 1, i 5, i 79, i 83
  , i 2, i 83, i 6, i 87
  , i 1, i 5, i 87, i 91
  , i 1, i 6, i 91, i 95
  , i 2, i 95, i 6, i 99
  , i 1, i 5, i 99, i 103
  , i 1, i 6, i 103, i 107
  , i 1, i 107, i 2, i 111
  , i 1, i 111, i 5, i 0
  , i 99
  , i 2, i 14, i 0 , i 0
  ]

-}
