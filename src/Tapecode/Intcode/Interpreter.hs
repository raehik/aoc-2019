{-# LANGUAGE TypeFamilies #-}

-- | Extensions for Intcode interpreters.
--
-- The Intcode specification has a clear step-based execution model, so we're
-- able to provide the top-level behaviour and build an interpreter given a
-- "step" function (i.e. "attempt to execute instruction at current position").

module Tapecode.Intcode.Interpreter where

import           Prelude hiding (read)

import           Tapecode.Tape
import           Tapecode.Interpreter
import qualified Tapecode.Intcode.Instruction as Instr
import           Tapecode.Intcode.Instruction ( Instruction(..), ParamMode(..) )

-- | ? TODO.
data Error
  = ErrInstructionErr Instr.Error
  | ErrUnimplemented
    deriving (Eq, Show)

-- | Outcome of attempting a step.
data Step
  = Step            -- ^ successful regular step
  | StepHalt        -- ^ step to end execution
  | StepErr Error   -- ^ some error during step
    deriving (Eq, Show)

data Result
  = OK              -- ^ execution ended successfully
  | ExecError Error -- ^ execution terminated early due to error
    deriving (Eq, Show)

-- | Given a step function, interpret the configured tape until halt or error.
execWithStep :: Monad m => m Step -> m Result
execWithStep step =
    step >>= \case
      Step        -> execWithStep step
      StepHalt    -> return OK
      StepErr err -> return (ExecError err)

-- | Continue execution (end a step).
--
-- Alias for @return Step@.
continue :: Monad m => m Step
continue = return Step

-- | Given an action to retrieve an Intcode-compatible type from a tape symbol,
--   and an instruction handler, interpret the configured tape until halt or
--   error.
--
-- This means any interpreter can be implemented solely by specifying how to
-- recover an 'Int' from a 'Symbol t', and what to do for each 'Instruction
-- ParamMode'. Powerful!
execWithInstrHandler
    :: (MonadInterp m, InterpTape m ~ t, Integral a)
    => (Symbol t -> m (Maybe a)) -> (Instruction ParamMode -> m Step)
    -> m Result
execWithInstrHandler tryGetIntcodeSym handleInstr = do
    sym <- read
    mIntcodeSym <- tryGetIntcodeSym sym
    case mIntcodeSym of
      Nothing         -> return $ ExecError ErrUnimplemented -- TODO
      Just intcodeSym ->
        case Instr.decode intcodeSym of
          Left err    -> return $ ExecError (ErrInstructionErr err)
          Right instr ->
            handleInstr instr >>= \case
              Step        -> execWithInstrHandler tryGetIntcodeSym handleInstr
              StepHalt    -> return OK
              StepErr err -> return $ ExecError err
