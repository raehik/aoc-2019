module Intcode.Programs.Symbolic where

import           Intcode.Interpreter.Symbolic
import           Intcode.Interpreter.Symbolic.MvarPoly
import qualified Data.Map.Lazy as Map

i :: a -> Sym a
i = SymConst

v :: Var -> Sym a
v x = SymExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

progD2SimilarSymbolicImm :: [Sym Int]
progD2SimilarSymbolicImm =
  [ i 1101, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]

progD2SimilarSymbolicPos :: [Sym Int]
progD2SimilarSymbolicPos =
  [ i 0001, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
