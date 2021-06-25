module Tapecode.Intcode.Tmp.ExecTestsSymbolic where

import           Tapecode.Machine
import           Tapecode.Intcode.Interpreter.Symbolic
import           Tapecode.Intcode.Interpreter.Symbolic.MvarPoly
import           Tapecode.Tape.IntMap

import           Control.Monad.State.Lazy

tmpExecSymProg :: [Sym Int] -> IO ()
tmpExecSymProg prog = do
    let initState = fromListToIdxIntMap 0 prog
    (result, endState) <- runStateT (runIOTapeMachine exec) initState
    print result
    let tapeOut = idxIntMapTape endState
        SymExp tapeOutIdx0Expr = head tapeOut
    print $ mvarPolyExpr tapeOutIdx0Expr
