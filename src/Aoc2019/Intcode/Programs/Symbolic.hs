module Aoc2019.Intcode.Programs.Symbolic where

import           Prelude hiding (read)
import           Aoc2019.Intcode.Interpreter.Symbolic
import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly
import qualified Data.Map.Lazy as Map

i :: a -> Sym a
i = SymConst

v :: Var -> Sym a
v x = SymExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

progD2SimilarSymbolic :: [Sym Int]
progD2SimilarSymbolic =
  [ i 1101, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
