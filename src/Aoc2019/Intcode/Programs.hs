{-# LANGUAGE TypeFamilies #-}

module Aoc2019.Intcode.Programs where

import           Prelude hiding (read)
import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Interpreter.Default
import           Aoc2019.Intcode.Tape
import           Aoc2019.Parsers

import           Control.Monad.State.Lazy

d2Machine
    :: (MonadInterp m, InterpTape m ~ t, Num a, Eq a, Symbol t ~ a, Index t ~ a)
    => m a
d2Machine = do
    next
    write 12
    next
    write 2
    moveLeftmost
    interpExec
    moveLeftmost
    read

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

--------------------------------------------------------------------------------

d2TestProgramLong :: [Int]
d2TestProgramLong =
  [ 1,9,10,3
  , 2,3,11,0
  , 99
  , 30,40,50
  ]

d2TestProgramLongOut :: [Int]
d2TestProgramLongOut =
  [ 3500,9,10,70
  , 2,3,11,0
  , 99
  , 30,40,50
  ]

d2TestProgramShortAdd, d2TestProgramShortAddOut :: [Int]
d2TestProgramShortAdd    = [ 1,0,0,0,99 ]
d2TestProgramShortAddOut = [ 2,0,0,0,99 ]
