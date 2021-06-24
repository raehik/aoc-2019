{-# LANGUAGE TypeFamilies #-}

module Aoc2019.Intcode.Interpreter.SymTest where

import           Prelude hiding (read)

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import           Aoc2019.Intcode.Tape.IntMapFixedPoint
import qualified Aoc2019.Intcode.Instruction.Int as Instr
import           Aoc2019.Intcode.Instruction.Int ( Instruction(..)
                                                 , ParamMode(..)
                                                 )

import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly

import qualified Data.Map.Lazy as Map

data Sym
  = SymConst Int
  | SymExp (MvarPoly Var Int Int)
    deriving (Eq, Ord, Show)

type Sym' = Either (IdxIntMapFP Sym) Sym

exec
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => m Result
exec = execWithStep step

step
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => m Step
step = do
    sym <- read
    case sym of
      Left _ ->
        -- tried to execute a recursive tape - no farking clue
        return $ StepErr ErrUnimplemented
      Right sym' ->
        case sym' of
          SymExp _ ->
            -- tried to execute an expr -- speculative execution?
            return $ StepErr ErrUnimplemented
          SymConst sym'' ->
            case Instr.decode sym'' of
              Left err    -> return $ StepErr (ErrInstructionErr err)
              Right instr -> handleInstr instr

-- | Handle an Intcode instruction.
handleInstr
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => Instruction ParamMode -> m Step
handleInstr = \case
  Add im1 im2 om -> stepBinop symAdd im1 im2 om
  Mul im1 im2 om -> stepBinop symMul im1 im2 om
  Hlt            -> return StepHalt
  _              -> return $ StepErr ErrUnimplemented

stepBinop
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => (Sym' -> Sym' -> Sym')
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
      Left _ ->
        -- tried to write to recursive tape TODO what does this _mean_ though??
        return $ StepErr ErrUnimplemented
      Right o' ->
        case o' of
          SymExp _ ->
            -- tried to write to plain expr -- seems impossible to solve
            return $ StepErr ErrUnimplemented
          SymConst o'' ->
            case om of
              PosMode -> do
                jump o''
                write (i1v `f` i2v)
                jump nextPos
                continue
              RelMode -> error "relmode yet unimplemented"
              ImmMode -> error "write parameter in immediate mode not allowed"

getParamValue
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => Sym' -> ParamMode -> m Sym'
getParamValue sym _ = return sym
{-
    \case
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
-}

-- TODO
symAdd :: Either (IdxIntMapFP Sym) Sym -> Either (IdxIntMapFP Sym) Sym -> Either (IdxIntMapFP Sym) Sym
symAdd x _ = x

symMul :: Either (IdxIntMapFP Sym) Sym -> Either (IdxIntMapFP Sym) Sym -> Either (IdxIntMapFP Sym) Sym
symMul x _ = x

--------------------------------------------------------------------------------

{-
tmpSymtestProgTestImm :: [Sym t]
tmpSymtestProgTestImm = map SymPlain
  [ i 1101, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

tmpSymtestProgTestPos :: [Sym t]
tmpSymtestProgTestPos = map SymPlain
  [ i 0001, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]
tmpSymtestProgTestImm :: [Sym t]
tmpSymtestProgTestImm = map SymPlain
  [ i 1101, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

tmpSymtestProgTestPos :: [Sym t]
tmpSymtestProgTestPos = map SymPlain
  [ i 0001, v "noun", v "verb", i 0
  , i 99
  , i 5, i 6, i 7, i 8, i 9
  ]
  where
    i   = ValConst
    v x = ValExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]
-}
