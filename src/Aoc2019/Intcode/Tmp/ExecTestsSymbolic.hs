module Aoc2019.Intcode.Tmp.ExecTestsSymbolic where

import           Aoc2019.Intcode.Machine
import           Aoc2019.Intcode.Interpreter.Symbolic
import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly
import           Aoc2019.Intcode.Tape.IntMap

import           Control.Monad.State.Lazy

tmpExecSymProg :: [Sym Int] -> IO ()
tmpExecSymProg prog = do
    let initState = fromListToIdxIntMap prog
    (result, endState) <- runStateT (runIOTapeMachine exec) initState
    print result
    let tapeIn  = idxIntMapTape initState
        tapeOut = idxIntMapTape endState
        SymExp tapeOutIdx0Expr = head tapeOut
    print $ mvarPolyExpr tapeOutIdx0Expr
