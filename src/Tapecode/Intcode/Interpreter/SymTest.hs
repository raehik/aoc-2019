{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Tapecode.Intcode.Interpreter.SymTest where

import           Prelude hiding (read)

import           Tapecode.Interpreter
import           Tapecode.Intcode.Interpreter
import           Tapecode.Tape.IntMapFixedPoint
import qualified Tapecode.Intcode.Instruction.Int as Instr
import           Tapecode.Intcode.Instruction.Int ( Instruction(..)
                                                 , ParamMode(..)
                                                 )

import           Tapecode.Intcode.Interpreter.Symbolic.MvarPoly

import qualified Data.Map.Lazy as Map

data Sym
  = SymConst Int
  | SymExp (MvarPoly Var Int Int)
    deriving (Eq, Ord, Show)

type Sym' = Either (Sym, IdxIntMapFP Sym) Sym

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
  Add im1 im2 om -> stepBinop sym'Add im1 im2 om
  Mul im1 im2 om -> stepBinop sym'Mul im1 im2 om
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
                let outval = i1v `f` i2v
                write outval
                jump nextPos
                continue
              RelMode -> error "relmode yet unimplemented"
              ImmMode -> error "write parameter in immediate mode not allowed"

getParamValue
    :: (MonadInterp m, InterpTape m ~ IdxIntMapFP Sym)
    => Sym' -> ParamMode -> m Sym'
getParamValue sym = \case
  PosMode ->
    case sym of
      Left _ -> error "attempted to use recursive tape as position"
      Right sym' ->
        case sym' of
          SymExp expr -> do
            t <- fullTape
            return $ Left (SymExp expr, t)
          SymConst ptr -> jump ptr >> read
  ImmMode -> return sym
  RelMode -> error "unimplemented" -- TODO

-- TODO
-- shouldn't matter which we pick when adding? maybe?
sym'Add :: Sym' -> Sym' -> Sym'
sym'Add (Right x) (Right y) = Right $ symAdd x y
sym'Add (Right x) (Left (yexp, y)) = Left (symAdd x yexp, y)
sym'Add x@(Left _) y@(Right _) = sym'Add y x
sym'Add (Left (xexp, x)) (Left (yexp, _)) = Left (symAdd xexp yexp, x)

sym'Mul :: Sym' -> Sym' -> Sym'
sym'Mul (Right x) (Right y) = Right $ symMul x y
sym'Mul (Right x) (Left (yexp, y)) = Left (symMul x yexp, y)
sym'Mul x@(Left _) y@(Right _) = sym'Mul y x
sym'Mul (Left (xexp, x)) (Left (yexp, _)) = Left (symMul xexp yexp, x)

symAdd :: Sym -> Sym -> Sym
symAdd (SymConst x) (SymConst y) = SymConst (x + y)
symAdd (SymConst x)           y  = symAdd (SymExp (mvarConstExpr x)) y
symAdd           x  (SymConst y) = symAdd x (SymExp (mvarConstExpr y))
symAdd (SymExp   x) (SymExp   y) = SymExp (mvarPolyAdd x y)

symMul :: Sym -> Sym -> Sym
symMul (SymConst x) (SymConst y) = SymConst (x * y)
symMul (SymConst x)           y  = symMul (SymExp (mvarConstExpr x)) y
symMul           x  (SymConst y) = symMul x (SymExp (mvarConstExpr y))
symMul (SymExp   x) (SymExp   y) = SymExp (mvarPolyMul x y)


--------------------------------------------------------------------------------

i :: Int -> Sym
i   = SymConst
v :: Var -> Sym
v x = SymExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

tmpSymtestProgTestMiniD2 :: [Sym]
tmpSymtestProgTestMiniD2 =
  [ i 1, v "noun", v "verb", i 0
  , i 1, i 0, i 9, i 0
  , i 99
  , i 2
  ]

tmpSymtestProgTestMiniD2Shorter :: [Sym]
tmpSymtestProgTestMiniD2Shorter =
  [ i 1, v "noun", v "verb", i 0
  , i 2, i 0, i 9, i 0
  , i 99
  , i 2
  ]
