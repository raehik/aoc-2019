{-# LANGUAGE TypeFamilies #-}

module Intcode.Tmp.ExecTestsOld where

{-
import           Prelude hiding (read)
import           Intcode.Interpreter
import           Intcode.Interpreter.Default
import           Intcode.Tape
import           Parsers

import           Control.Monad.State.Lazy

tmpInitState :: FilePath -> IO (IdxIntMap Int)
tmpInitState fp = do
    x <- parseAndUseResource fp pIntcodeFile id
    case x of
      Nothing -> error "fuck"
      Just y  -> return $ fromListToIdxIntMap y

test :: IO ()
test = do
    initState <- tmpInitState "d2.txt"
    (_, outmap) <- runStateT (runIOTapeMachine d2Machine) initState
    print $ idxIntMapTape outmap

testStatic :: [Int] -> [Int] -> IO ()
testStatic prog expected = do
    let initState = fromListToIdxIntMap prog
    (_, endState) <- runStateT (runIOTapeMachine interpExec) initState
    let tapeIn  = idxIntMapTape initState
        tapeOut = idxIntMapTape endState
    print tapeIn
    print tapeOut
    print expected
    if tapeOut == expected then putStrLn "sick" else putStrLn "meh ok"
-}
