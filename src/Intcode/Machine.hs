-- | Concrete Intcode machine to execute supported interpreters.

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Intcode.Machine where

import           Prelude hiding (read)

import           Intcode.Interpreter
import           Intcode.Tape
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

    fullTape = get
