module Aoc2019.Intcode.Tmp.ExecTestsSymbolic2 where

import           Aoc2019.Intcode.MachineSym
import           Aoc2019.Intcode.Interpreter.SymTest
import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly
import           Aoc2019.Intcode.Tape.IntMapFixedPoint

import           Control.Monad.State.Lazy
import qualified Data.Text.Lazy as Text
import qualified Data.Map.Lazy as Map

import           Aoc2019.Intcode.Interpreter

tmpExecSymtestProg :: [Sym] -> IO ()
tmpExecSymtestProg prog = do
    let initState = fromListToIdxIntMapFP prog
    (result, endState) <- runStateT (runIOTapeMachineFP exec) initState
    print result
    let tapeIn  = idxIntMapFPTape initState
        tapeOut = idxIntMapFPTape endState
        tapeOutIdx0 = head tapeOut
    print tapeOutIdx0
    print tapeOut

--------------------------------------------------------------------------------

progD2P2Symbolic' :: [Sym]
progD2P2Symbolic' =
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
  where
    i   = SymConst
    v x = SymExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

{-
exec :: IOTapeMachineFP Result
exec = execWithStep step
  where
    -- | Given a step function, run the machine on the configured program until HALT
    --   or error.
    execWithStepManual step =
        step >>= \case
          Step        -> execWithStep step
          StepHalt    -> return OK
          StepErr err -> return (ExecError err)
-}
