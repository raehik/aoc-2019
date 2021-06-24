-- | DSL for writing Intcode interpreters ("tape machines").

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.Intcode.Interpreter.Default where

import           Prelude hiding (read)

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import           Control.Monad.State.Lazy

newtype IOTapeMachine t a = IOTapeMachine
  { runIOTapeMachine :: StateT t IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState t
             )

--updateTapeOrContinueWithIOLog :: (MonadIO m, MonadInterp m) => Maybe (InterpTape m) -> m ()
updateTapeOrContinueWithIOLog :: Maybe (InterpTape (IOTapeMachine t)) -> IOTapeMachine t ()
updateTapeOrContinueWithIOLog = \case
  Nothing    -> liftIO $ putStrLn "program error, ignoring"
  Just tape' -> put tape'

instance Tape t => MonadInterp (IOTapeMachine t) where
    type InterpTape (IOTapeMachine t) = t
    next = get >>= updateTapeOrContinueWithIOLog . tapeNext
    prev = get >>= updateTapeOrContinueWithIOLog . tapePrev
    read = get >>= return . tapeRead
    write a = modify (tapeWrite a)
    readPos = get >>= return . tapePos
    jump idx = get >>= updateTapeOrContinueWithIOLog . tapeJump idx

    -- TODO: This is where the efficiency starts to break down, and we need to
    -- start writing more concrete instances with type Index t = Index, and
    -- perhaps amending the Tape class or making extension Tape classes.
    moveLeftmost = do
        tape <- get
        case tapePrev tape of
          Nothing    -> return ()
          Just tape' -> put tape' >> moveLeftmost
    moveRightmost = do
        tape <- get
        case tapeNext tape of
          Nothing    -> return ()
          Just tape' -> put tape' >> moveRightmost

stop :: Monad m => m ()
stop = return ()

-- TODO: could add MonadIO m and change 'error' to 'liftIO (putStrLn' ...)
interpExec
    :: (MonadInterp m, InterpTape m ~ t, Num a, Eq a, Symbol t ~ a, Index t ~ a)
    => m ()
interpExec = do
    opcode <- read
    case opcode of
      99 -> stop
      1  -> interpPtrBinop (+) >> interpExec
      2  -> interpPtrBinop (*) >> interpExec
      _  -> error "unknown opcode" >> stop

-- | Holy shit rofl, check this out. Look at how generic this is. This builds a
--   binop opcode interpreter for *any interpreter which uses tape with Num-like
--   symbols and allows searching via Num-likes*. This is literally as abstract
--   as you can get sensibly.
interpPtrBinop
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ a, Index t ~ a)
    => (a -> a -> a) -> m ()
interpPtrBinop f = do
    curPos <- readPos
    let nextPos = curPos + 4
    next
    in1 <- read
    next
    in2 <- read
    next
    out <- read
    jump in1
    in1v <- read
    jump in2
    in2v <- read
    jump out
    write (in1v `f` in2v)
    jump nextPos
