-- | Sadly, I've fucked up the tape data type, so none of this matters. I've
--   made it so that symbols can contain other symbols -- I actually need it so
--   that symbols can contain a whole 'nother tape. I guess I'll have a quick
--   go, but I don't see how I can even write the data type.

{-# LANGUAGE TypeFamilies #-}

module Aoc2019.Intcode.Interpreter.SymTest where

import           Prelude hiding (read)

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import qualified Aoc2019.Intcode.Instruction.Int as Instr
import           Aoc2019.Intcode.Instruction.Int ( Instruction(..)
                                                 , ParamMode(..)
                                                 )

import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly

import qualified Data.Map.Lazy as Map

data Val
  = ValConst Int
  | ValExp   (MvarPoly Var Int Int)
    deriving (Eq, Ord, Show)

data Sym
  = SymPlain    Val     -- ^ always val
  | SymSnapshot Val Sym -- ^ val at this point in time
    deriving (Eq, Ord, Show)

symVal :: Sym -> Val
symVal = \case
  SymPlain    v   -> v
  SymSnapshot v _ -> v

exec
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym, Index t ~ Int)
    => m Result
exec = execWithStep step

step
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym, Index t ~ Int)
    => m Step
step = do
    sym <- read
    case symVal sym of
      ValConst sym' ->
        case Instr.decode sym' of
          Left err    -> return $ StepErr (ErrInstructionErr err)
          Right instr -> handleInstr instr
      ValExp _ -> return $ StepErr ErrUnimplemented  -- can't exec exprs

-- | Handle an Intcode instruction.
handleInstr
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym, Index t ~ Int)
    => Instruction ParamMode -> m Step
handleInstr = \case
  Add im1 im2 om -> stepBinop symAdd im1 im2 om
  Mul im1 im2 om -> stepBinop symMul im1 im2 om
  Hlt            -> return StepHalt
  _              -> return $ StepErr ErrUnimplemented

stepBinop
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym, Index t ~ Int)
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
    i1v <- getParamValue i1 im1
    i2v <- getParamValue i2 im2
    case o of
      SymSnapshot _ _ ->
        -- tried to write to snapshot TODO what does this _mean_ though??
        return $ StepErr ErrUnimplemented
      SymPlain val ->
        case val of
          ValExp _ ->
            -- tried to write to plain expr -- seems impossible to solve
            return $ StepErr ErrUnimplemented
          ValConst o' ->
            case om of
              PosMode -> do
                jump o'
                write (i1v `f` i2v)
                jump nextPos
                continue
              RelMode -> error "relmode yet unimplemented"
              ImmMode -> error "write parameter in immediate mode not allowed"

getParamValue
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym, Index t ~ Int)
    => Symbol t -> ParamMode -> m (Symbol t)
getParamValue sym = \case
  PosMode ->
    case sym of
      SymPlain val ->
        case val of
          ValConst ptr -> jump ptr >> read
          ValExp _     -> return $ SymSnapshot val sym
      SymSnapshot val _ ->
        case val of
          ValConst ptr -> jump ptr >> read -- TODO: is this reachable?
          ValExp _     -> error "attempted to use expr snapshot as position"
  ImmMode -> return sym
  RelMode -> error "unimplemented" -- TODO

symAdd :: Sym -> Sym -> Sym
symAdd (SymPlain x) (SymPlain y) = SymPlain (x `valAdd` y)
symAdd (SymPlain x) (SymSnapshot y yss) = SymSnapshot (x `valAdd` y) yss
symAdd (SymSnapshot x xss) (SymPlain y) = symAdd (SymPlain y) (SymSnapshot x xss)
-- TODO prioritising left... rofl this is bs
symAdd (SymSnapshot x xss) (SymSnapshot y _) = SymSnapshot (x `valAdd` y) xss

symMul :: Sym -> Sym -> Sym
symMul (SymPlain x) (SymPlain y) = SymPlain (x `valMul` y)
symMul (SymPlain x) (SymSnapshot y yss) = SymSnapshot (x `valMul` y) yss
symMul (SymSnapshot x xss) (SymPlain y) = symMul (SymPlain y) (SymSnapshot x xss)
-- TODO prioritising left... rofl this is bs
symMul (SymSnapshot x xss) (SymSnapshot y _) = SymSnapshot (x `valMul` y) xss

valAdd :: Val -> Val -> Val
valAdd (ValConst x) (ValConst y) = ValConst (x + y)
valAdd (ValConst x) (ValExp   (MvarPoly y)) = ValExp (MvarPoly (Map.map (+x) y))
valAdd (ValExp   x) (ValConst y) = valAdd (ValConst y) (ValExp x)
valAdd (ValExp   x) (ValExp   y) = ValExp (mvarPolyAdd x y)

valMul :: Val -> Val -> Val
valMul (ValConst x) (ValConst y) = ValConst (x * y)
valMul (ValConst x) (ValExp   (MvarPoly y)) = ValExp (MvarPoly (Map.map (*x) y))
valMul (ValExp   x) (ValConst y) = valMul (ValConst y) (ValExp x)
valMul (ValExp   x) (ValExp   y) = ValExp (mvarPolyMul x y)

--------------------------------------------------------------------------------

tmpSymtestProgTestImm :: [Sym]
tmpSymtestProgTestImm = map SymPlain
  [ i 1101, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

tmpSymtestProgTestPos :: [Sym]
tmpSymtestProgTestPos = map SymPlain
  [ i 0001, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]
