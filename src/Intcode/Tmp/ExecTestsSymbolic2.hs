module Intcode.Tmp.ExecTestsSymbolic2 where

import           Intcode.MachineSym
import           Intcode.Interpreter.SymTest
import           Intcode.Interpreter.Symbolic.MvarPoly
import           Intcode.Tape.IntMapFixedPoint

import           Control.Monad.State.Lazy
import qualified Data.Text.Lazy.IO as Text

tmpExecSymtestProg :: [Sym] -> IO ()
tmpExecSymtestProg prog = do
    let initState = fromListToIdxIntMapFP prog
    (result, endState) <- runStateT (runIOTapeMachineFP exec) initState
    print result
    let tapeOut = idxIntMapFPTape endState
        tapeOutIdx0 = head tapeOut
        SymExp tmp = tapeOutIdx0
        Just tmp' = mvarPolyExpr' id tmp
    Text.putStrLn tmp'
    print tapeOutIdx0
    print tapeOut
    print endState

tmpExecSymtestProg' :: [Sym] -> IO (IdxIntMapFP Sym)
tmpExecSymtestProg' prog = do
    let initState = fromListToIdxIntMapFP prog
    (_, endState) <- runStateT (runIOTapeMachineFP exec) initState
    return endState

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

progD2P2Symbolic'Solved :: [Sym]
progD2P2Symbolic'Solved =
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
