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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.Intcode.Interpreter.Symbolic where

import           Prelude hiding (read)

import           Aoc2019.Utils
import           Aoc2019.Intcode.Tape
import           Aoc2019.Intcode.Interpreter
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder
import           Control.Monad.IO.Class

-- TODO: pull MvarPoly into its own module, and give it a typevar (which will
-- normally be constrained to Num, concretely Int and Double)

type Var = Text
type MvarPoly = Map (Map Var Int) Int

-- | Multiply two 'MvarPoly's together. Does not attempt to retain normal form.
mvarPolyMul :: MvarPoly -> MvarPoly -> MvarPoly
mvarPolyMul mL = Map.foldrWithKey go Map.empty
  where
    go :: Map Var Int -> Int -> MvarPoly -> MvarPoly
    go polyn coeff mComb = Map.foldrWithKey (goInner polyn coeff) mComb mL
    goInner :: Map Var Int -> Int -> Map Var Int -> Int -> MvarPoly -> MvarPoly
    goInner polynL coeffL polynR coeffR mComb =
        let inner = Map.unionWith (+) polynL polynR
            coeff = coeffL * coeffR
         in Map.insertWith (+) inner coeff mComb

-- | Add two 'MvarPoly's together. Does not attempt to retain normal form.
mvarPolyAdd :: MvarPoly -> MvarPoly -> MvarPoly
mvarPolyAdd = Map.unionWith (+)

data Sign
  = SignPos
  | SignNeg

pprintSign :: Sign -> Text
pprintSign = \case
  SignPos -> "+"
  SignNeg -> "-"

-- Takes an MvarPoly in normal form (no 0 coeffs, no 0 powers). Will print
-- "superfluous" terms if not in normal form. (Does handle empty expressions,
-- however, since we need to for good sign behaviour anyway.)
mvarPolyExpr :: MvarPoly -> Maybe Text
mvarPolyExpr = fmap toLazyText . Map.foldrWithKey buildExpr Nothing
  where
    buildExpr :: Map Var Int -> Int -> Maybe Builder -> Maybe Builder
    buildExpr polynMap coeff = \case
      Nothing  ->
        case sign of
          SignPos -> Just term
          SignNeg -> Just $ fromLazyText (pprintSign sign) <> term
      Just bld -> Just $ bld <> " " <> fromLazyText (pprintSign sign) <> " " <> term
      where
        polyn = (Map.foldrWithKey buildPolyPart mempty) polynMap
        sign  = if coeff < 0 then SignNeg else SignPos
        term  = if coeff == 1 then polyn else fromLazyText (tshow (abs coeff)) <> polyn
    buildPolyPart :: Var -> Int -> Builder -> Builder
    buildPolyPart var pwr bld
      | pwr == 1  = bld <> fromLazyText var
      | otherwise = bld <> fromLazyText var <> "^" <> fromLazyText (tshow pwr)

data Sym a
  = SymConst a
  | SymExp MvarPoly
  -- | SymPtr Int
    deriving (Eq, Ord, Show)

stop :: Monad m => m ()
stop = return ()

interpSymbolicExec
    :: (MonadInterp m, MonadIO m, InterpTape m ~ t, Symbol t ~ Sym Int, Index t ~ Int)
    => m ()
interpSymbolicExec = do
    opcode <- read
    case opcode of
      SymExp _   -> liftIO (putStrLn "tried to execute an expression") >> stop
      SymConst x ->
        case x of
          99 -> stop
          1  -> interpSymbolicPtrBinop symAdd >> interpSymbolicExec
          2  -> interpSymbolicPtrBinop symMul >> interpSymbolicExec
          _  -> liftIO (putStrLn "unknown opcode") >> stop

interpSymbolicPtrBinop
    :: (MonadInterp m, MonadIO m, InterpTape m ~ t, Symbol t ~ Sym Int, Index t ~ Int)
    => (Sym Int -> Sym Int -> Sym Int) -> m ()
interpSymbolicPtrBinop f = do
    curPos <- readPos
    let nextPos = curPos + 4
    next
    in1 <- read
    next
    in2 <- read
    next
    out <- read
    case in1 of
      SymExp _        -> liftIO (putStrLn "tried to jump to an expression") >> attemptStop
      SymConst in1ptr -> do
        jump in1ptr
        in1v <- read
        case in2 of
          SymExp _        -> liftIO (putStrLn "tried to jump to an expression") >> attemptStop
          SymConst in2ptr -> do
            jump in2ptr
            in2v <- read
            case out of
              SymExp _      -> liftIO (putStrLn "tried to jump to an expression") >> attemptStop
              SymConst outptr -> do
                jump outptr
                write (in1v `f` in2v)
                jump nextPos

symMul :: Sym Int -> Sym Int -> Sym Int
symMul (SymConst x) (SymConst y) = SymConst (x * y)
symMul (SymConst x) (SymExp   y) = SymExp (Map.map (*x) y)
symMul (SymExp   x) (SymConst y) = symMul (SymConst y) (SymExp x)
symMul (SymExp   x) (SymExp   y) = SymExp (mvarPolyMul x y)

symAdd :: Sym Int -> Sym Int -> Sym Int
symAdd (SymConst x) (SymConst y) = SymConst (x + y)
symAdd (SymConst x) (SymExp   y) = SymExp (Map.map (+x) y)
symAdd (SymExp   x) (SymConst y) = symAdd (SymConst y) (SymExp x)
symAdd (SymExp   x) (SymExp   y) = SymExp (mvarPolyAdd x y)

-- Overwrite current pointer with halt opcode, and hope that the next thing that
-- happens is regular execution.
attemptStop
    :: (MonadInterp m, InterpTape m ~ t, Num a, Symbol t ~ Sym a)
    => m ()
attemptStop = write (SymConst 99)

--------------------------------------------------------------------------------

exProg :: [Sym Int]
exProg =
  [ i 2,  i 13,  i 14, i 0
  , i 2,  i 0,   i 13, i 0
  , i 1,  i 0,   i 15, i 0
  , i 99, v "a", v "b", v "c"
  ]
  where
    i = SymConst
    v x = SymExp $ Map.fromList [(Map.fromList [(x, 1)], 1)]

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
mvarPolyEx1 :: MvarPoly
mvarPolyEx1 = Map.fromList
  [ ( Map.fromList [ ("x", 2) ] , 1)
  , ( Map.fromList [ ("x", 1) ] , 2)
  , ( Map.fromList []           , (-1))
  ]

-- | x + 1
mvarPolyEx2 :: MvarPoly
mvarPolyEx2 = Map.fromList
  [ ( Map.fromList [ ("x", 1) ] , 1)
  , ( Map.fromList []           , 1)
  ]

-- | x + z + 2
mvarPolyEx3 :: MvarPoly
mvarPolyEx3 = Map.fromList
  [ ( Map.fromList [ ("x", 1) ] , 1)
  , ( Map.fromList [ ("z", 1) ] , 1)
  , ( Map.fromList []           , 2)
  ]
