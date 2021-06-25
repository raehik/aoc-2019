module Tapecode.Intcode.Tmp.ExecTests where

import           Tapecode.Machine
import           Tapecode.Intcode.Interpreter.Default
import           Tapecode.Tape.IntMap

import           Control.Monad.State.Lazy

tmpExecProgViaStep :: [Int] -> IO ()
tmpExecProgViaStep prog = do
    let initState = fromListToIdxIntMap 0 prog
    (result, endState) <- runStateT (runIOTapeMachine exec) initState
    print result
    let tapeIn  = idxIntMapTape initState
        tapeOut = idxIntMapTape endState
    print tapeIn
    print tapeOut
