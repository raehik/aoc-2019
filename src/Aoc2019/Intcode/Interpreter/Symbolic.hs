-- | Symbolic Intcode interpreter, supporting algebraic operations.
--
-- The Intcode spec defines code, data and pointers as one and the same.
-- Allowing algebraic expressions to impact control flow is challenging (we must
-- attempt to solve the system at that point, and potentially test multiple
-- paths), so we restrict expressions to _data only_. Only constants can be
-- interpreted as opcodes -- if the interpreter attempts to execute an algebraic
-- expression, it errors out. Using an expression as a pointer is also
-- forbidden (again a terribly ugly state of affairs).
--
-- Perhaps the above would be more possible if I implemented step semantics.
-- Which shouldn't be particularly hard.

{-# LANGUAGE TypeFamilies               #-}

module Aoc2019.Intcode.Interpreter.Symbolic where

import           Prelude hiding (read)

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import qualified Aoc2019.Intcode.Instruction.Int as Instr
import           Aoc2019.Intcode.Instruction.Int ( Instruction(..)
                                                 , ParamMode(..)
                                                 )

import           Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly

import qualified Data.Map.Lazy as Map

data Sym a
  = SymConst a
  | SymExp (MvarPoly Int Int)
  -- | SymPtr Int
    deriving (Eq, Ord, Show)

exec
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym Int, Index t ~ Int)
    => m Result
exec = execWithStep step

step
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym Int, Index t ~ Int)
    => m Step
step = do
    sym <- read
    case sym of
      SymExp _   -> return $ StepErr ErrUnimplemented   -- can't exec exprs
      SymConst sym' ->
        case Instr.decode sym' of
          Left err    -> return $ StepErr (ErrInstructionErr err)
          Right instr -> handleInstr instr

-- | Handle an Intcode instruction.
handleInstr
    :: (MonadInterp m, InterpTape m ~ t, Symbol t ~ Sym Int, Index t ~ Int)
    => Instruction ParamMode -> m Step
handleInstr = \case
  Add im1 im2 om -> stepBinop symAdd im1 im2 om
  Mul im1 im2 om -> stepBinop symMul im1 im2 om
  Hlt            -> return StepHalt
  _              -> return $ StepErr ErrUnimplemented

stepBinop
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ Sym a, Index t ~ a)
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
      SymExp _    -> return $ StepErr ErrUnimplemented -- tried to write to expr
      SymConst o' -> do
        case om of
          PosMode -> do
            jump o'
            write (i1v `f` i2v)
            jump nextPos
            continue
          RelMode -> error "relmode yet unimplemented"
          ImmMode -> error "write parameter in immediate mode not allowed"

-- | TODO coolest fucking shit ever
getParamValue
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ Sym a, Index t ~ a)
    => Symbol t -> ParamMode -> m (Symbol t)
getParamValue sym = \case
  PosMode ->
    case sym of
      SymConst ptr -> jump ptr >> read
      SymExp _     -> error "unsure how to reference value at expr"
  ImmMode -> return sym
  RelMode -> error "unimplemented" -- TODO

symAdd :: Sym Int -> Sym Int -> Sym Int
symAdd (SymConst x) (SymConst y) = SymConst (x + y)
symAdd (SymConst x) (SymExp   (MvarPoly y)) = SymExp (MvarPoly (Map.map (+x) y))
symAdd (SymExp   x) (SymConst y) = symAdd (SymConst y) (SymExp x)
symAdd (SymExp   x) (SymExp   y) = SymExp (mvarPolyAdd x y)

symMul :: Sym Int -> Sym Int -> Sym Int
symMul (SymConst x) (SymConst y) = SymConst (x * y)
symMul (SymConst x) (SymExp   (MvarPoly y)) = SymExp (MvarPoly (Map.map (*x) y))
symMul (SymExp   x) (SymConst y) = symMul (SymConst y) (SymExp x)
symMul (SymExp   x) (SymExp   y) = SymExp (mvarPolyMul x y)

--------------------------------------------------------------------------------

progSymEx :: [Sym Int]
progSymEx =
  [ i 2,  i 13,  i 14, i 0
  , i 2,  i 0,   i 13, i 0
  , i 1,  i 0,   i 15, i 0
  , i 99, v "a", v "b", v "c"
  ]
  where
    i = SymConst
    v x = SymExp $ MvarPoly $ Map.fromList [(Map.fromList [(x, 1)], 1)]

{-
interpSymbolicTest :: [Sym Int] -> IO ()
interpSymbolicTest prog = do
    let initState = fromListToIdxIntMap prog
    endState <- execStateT (runIOTapeMachine interpSymbolicExec) initState
    let tapeIn    = idxIntMapTape initState
        tapeOut   = idxIntMapTape endState
    print tapeIn
    print tapeOut
    let SymExp loc0Expr     = head tapeOut
        Just loc0ExprPretty = mvarPolyExpr loc0Expr
    Text.putStrLn loc0ExprPretty
-}

--------------------------------------------------------------------------------

-- | x^2 + 2x - 1
mvarPolyEx1 :: MvarPoly Int Int
mvarPolyEx1 = MvarPoly $ Map.fromList
  [ ( Map.fromList [ ("x", 2) ] , 1)
  , ( Map.fromList [ ("x", 1) ] , 2)
  , ( Map.fromList []           , (-1))
  ]

-- | x + 1
mvarPolyEx2 :: MvarPoly Int Int
mvarPolyEx2 = MvarPoly $ Map.fromList
  [ ( Map.fromList [ ("x", 1) ] , 1)
  , ( Map.fromList []           , 1)
  ]

-- | x + z + 2
mvarPolyEx3 :: MvarPoly Int Int
mvarPolyEx3 = MvarPoly $ Map.fromList
  [ ( Map.fromList [ ("x", 1) ] , 1)
  , ( Map.fromList [ ("z", 1) ] , 1)
  , ( Map.fromList []           , 2)
  ]
