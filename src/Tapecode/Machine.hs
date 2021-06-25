-- | Concrete Tapecode machine to execute supported interpreters.

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tapecode.Machine where

import           Prelude hiding (read)

import           Tapecode.Tape
import           Tapecode.Interpreter
import           Control.Monad.State.Lazy

newtype IOTapeMachine t a = IOTapeMachine
  { runIOTapeMachine :: StateT t IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState t
             )

updateTapeOrContinueWithIOLog :: String -> Maybe (InterpTape (IOTapeMachine t)) -> IOTapeMachine t ()
updateTapeOrContinueWithIOLog msg = \case
  Nothing    -> liftIO $ putStrLn $ "ignoring program error: " <> msg
  Just tape' -> put tape'

instance Tape t => MonadInterp (IOTapeMachine t) where
    type InterpTape (IOTapeMachine t) = t
    next = get >>= updateTapeOrContinueWithIOLog "right end of tape" . tapeNext
    prev = get >>= updateTapeOrContinueWithIOLog "left end of tape" . tapePrev
    read = tapeRead <$> get
    write a = modify (tapeWrite a)
    readPos = tapePos <$> get
    jump idx = get >>= updateTapeOrContinueWithIOLog "jumped off tape" . tapeJump idx

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
    annoGet = tapeAnnoGet <$> get
    annoSet = modify . tapeAnnoSet
