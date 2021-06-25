-- | Concrete Intcode machine to execute supported interpreters, intended for
-- the symbolic interpreter which requires special recursive tape.

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.Intcode.MachineSym where

import           Prelude hiding (read)

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import           Aoc2019.Intcode.Tape.IntMapFixedPoint
import           Control.Monad.State.Lazy

newtype IOTapeMachineFP t a = IOTapeMachineFP
  { runIOTapeMachineFP :: StateT (IdxIntMapFP t) IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState (IdxIntMapFP t)
             )

--updateTapeOrContinueWithIOLog :: (MonadIO m, MonadInterp m) => Maybe (InterpTape m) -> m ()
updateTapeOrContinueWithIOLog :: Maybe (InterpTape (IOTapeMachineFP t)) -> IOTapeMachineFP t ()
updateTapeOrContinueWithIOLog = \case
  Nothing    -> liftIO $ putStrLn "program error, ignoring"
  Just tape' -> put tape'

instance MonadInterp (IOTapeMachineFP t) where
    type InterpTape (IOTapeMachineFP t) = IdxIntMapFP t
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

    fullTape = get >>= return . tapeFull
