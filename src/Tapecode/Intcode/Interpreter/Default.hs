{-# LANGUAGE TypeFamilies #-}

module Tapecode.Intcode.Interpreter.Default where

import           Prelude hiding (read)

import           Tapecode.Tape
import           Tapecode.Interpreter
import           Tapecode.Intcode.Interpreter
import qualified Tapecode.Intcode.Instruction.Int as Instr
import           Tapecode.Intcode.Instruction.Int ( Instruction(..)
                                                  , ParamMode(..)
                                                  )

exec
    :: (MonadInterp m, InterpTape m ~ t, Integral a, Symbol t ~ a, Index t ~ a)
    => m Result
--exec = execWithStep step
exec = execWithInstrHandler (return . Just) handleInstr

-- | Execute a single machine step (run a single instruction).
--
-- We impose minimal restrictions on the tape -- all we need is an Integral
-- instance to decode the Intcode instruction, and the tape symbol to be
-- compatible with the tape index to handle jumps. Essentially, we ask that the
-- tape is a single von Neumann-style "bus" (code and data look the same).
step
    :: (MonadInterp m, InterpTape m ~ t, Integral a, Symbol t ~ a, Index t ~ a)
    => m Step
step = do
    sym <- read
    case Instr.decode sym of
      Left err    -> return $ StepErr (ErrInstructionErr err)
      Right instr -> handleInstr instr

-- | Handle an Intcode instruction.
--
-- Since Intcode supports only minimal numeric operations, we're able to drop
-- the Integral requirement here down to a Num instead. Hooray for pointlessly
-- polymorphic code!
handleInstr
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ a, Index t ~ a)
    => Instruction ParamMode -> m Step
handleInstr = \case
  Add im1 im2 om -> stepBinop (+) im1 im2 om
  Mul im1 im2 om -> stepBinop (*) im1 im2 om
  Hlt            -> return StepHalt
  _              -> return $ StepErr ErrUnimplemented

stepBinop
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ a, Index t ~ a)
    => (Symbol t -> Symbol t -> Symbol t)
    -> ParamMode -> ParamMode -> ParamMode -> m Step
stepBinop f im1 im2 om = do
    curPos <- readPos
    let nextPos = curPos + 4
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
        jump nextPos
        continue
      RelMode -> error "relmode yet unimplemented"
      ImmMode -> error "write parameter in immediate mode not allowed"

-- | Get the value of a given parameter.
--
-- The end pointer depends on the parameter mode.
--
-- I think this is a great example of highly-polymorphic code simplifying
-- the process of writing definitions. Here, the function definition follows
-- naturally from the type:
--
--   * We need to return a symbol from the configured tape.
--   * We're given a symbol from the tape, a parameter mode, and the fact that
--     symbols and pointers are identical for this tape (i.e. pointers are
--     regular data).
--
-- We can retrieve the requested value in two ways: directly return the symbol
-- argument, or use the symbol as a pointer to read another symbol from the
-- tape. That decision is made by checking the parameter mode. For more
-- complicated tapes which use more complex symbols, you may have to do more
-- work to recover an index from your symbol (or it may be invalid).
--
-- Actually, yeah, this part deserves a write-up. This feels like the function I
-- was aiming to write all along.
getParamValue
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ a, Index t ~ a)
    => Symbol t -> ParamMode -> m (Symbol t)
getParamValue sym = \case
  PosMode -> jump sym >> read
  ImmMode -> return sym
  RelMode -> error "relmode yet unimplemented"

--------------------------------------------------------------------------------

-- | Execute special D2 machine (makes some tape writes first before regular
--   execution).
--
-- TODO: move later. Also, probably edit to do an post-Output.
execD2
    :: (MonadInterp m, InterpTape m ~ t, Integral a, Symbol t ~ a, Index t ~ a)
    => m Result
execD2 = do
    next
    write 12
    next
    write 2
    moveLeftmost
    exec
