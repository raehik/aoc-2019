module Intcode.Tmp.ExecTestsSymbolic where

import           Intcode.Machine
import           Intcode.Interpreter.Symbolic
import           Intcode.Interpreter.Symbolic.MvarPoly
import           Intcode.Tape.IntMap

import           Control.Monad.State.Lazy

tmpExecSymProg :: [Sym Int] -> IO ()
tmpExecSymProg prog = do
    let initState = fromListToIdxIntMap prog
    (result, endState) <- runStateT (runIOTapeMachine exec) initState
    print result
    let tapeOut = idxIntMapTape endState
        SymExp tapeOutIdx0Expr = head tapeOut
    print $ mvarPolyExpr tapeOutIdx0Expr
