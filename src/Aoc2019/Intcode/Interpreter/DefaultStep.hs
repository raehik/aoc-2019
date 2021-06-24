{-# LANGUAGE TypeFamilies #-}

module Aoc2019.Intcode.Interpreter.DefaultStep where

import           Prelude hiding (read)
import qualified Aoc2019.Intcode.Instruction.Int as Instr
import           Aoc2019.Intcode.Instruction.Int ( Instruction(..)
                                                 , ParamMode(..)
                                                 )
import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape

exec
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Int, Index t ~ Int)
    => m Result
exec = execWithStep step

-- | Execute a single machine step (run a single instruction)
step
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Int, Index t ~ Int)
    => m Step
step = do
    sym <- read
    case Instr.decode sym of
      Left err    -> return $ StepErr (ErrInstructionErr err)
      Right instr -> handleInstr instr

handleInstr
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Int, Index t ~ Int)
    => Instruction ParamMode -> m Step
handleInstr = \case
  Add im1 im2 om -> stepBinop (+) im1 im2 om
  Mul im1 im2 om -> stepBinop (*) im1 im2 om
  _              -> return $ StepErr ErrUnimplemented

stepBinop
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Int, Index t ~ Int)
    => (Symbol t -> Symbol t -> Symbol t)
    -> ParamMode -> ParamMode -> ParamMode -> m Step
stepBinop f im1 im2 om = do
    next
    i1 <- read
    next
    i2 <- read
    next
    o  <- read
    -- we read param modes then gather values in bulk, since if we gathered one
    -- by one we'd have to jump around more
    i1v <- getParamValue i1 im1
    i2v <- getParamValue i2 im2
    case om of
      PosMode -> do
        jump o
        write (i1v `f` i2v)
        continue
      RelMode -> error "unimplemented"
      ImmMode -> error "write parameter in immediate mode not allowed"

-- | Get the value of a given parameter.
--
-- The end pointer depends on the parameter mode.
getParamValue
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ a, Index t ~ a)
    => Symbol t -> ParamMode -> m (Symbol t)
getParamValue sym = \case
  PosMode -> do
    jump sym
    read
  ImmMode -> return sym
  RelMode -> error "unimplemented"
