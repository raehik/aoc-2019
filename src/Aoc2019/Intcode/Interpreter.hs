-- | DSL for writing Intcode interpreters ("tape machines").

{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Aoc2019.Intcode.Interpreter where

import           Prelude hiding (read)

import           Data.Kind
import           Aoc2019.Intcode.Tape
import qualified Aoc2019.Intcode.Instruction.Int as Instr
--import           Aoc2019.Intcode.Instruction.Int ( Instruction(..)
--                                                 , ParamMode(..)
--                                                 )

class (Monad m, Tape (InterpTape m)) => MonadInterp m where
    type InterpTape m :: Type
    next :: m ()
    prev :: m ()
    read :: m (Symbol (InterpTape m))
    write :: Symbol (InterpTape m) -> m ()
    jump :: Index (InterpTape m) -> m ()
    readPos :: m (Index (InterpTape m))
    moveLeftmost  :: m ()
    moveRightmost :: m ()

data Error
  = ErrInstructionErr Instr.Error
  | ErrUnimplemented
    deriving (Eq, Show)

data Step
  = Step            -- ^ regular step
  | StepHalt        -- ^ step to end execution
  | StepErr Error   -- ^ some error during step
    deriving (Eq, Show)

data Result
  = OK              -- ^ execution ended successfully
  | ExecError Error -- ^ execution terminated early due to error
    deriving (Eq, Show)

-- | Continue execution (end a step).
--
-- Alias for @return Step@.
continue :: Monad m => m Step
continue = return Step

-- | Given a step function, run the machine on the configured program until HALT
--   or error.
execWithStep :: Monad m => m Step -> m Result
execWithStep step =
    step >>= \case
      Step        -> execWithStep step
      StepHalt    -> return OK
      StepErr err -> return (ExecError err)
