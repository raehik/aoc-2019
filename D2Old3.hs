{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.D2 where

import           Control.Monad.State.Lazy

-- List zipper over a '[a]' with single annotation 'b'.
data IndexedListZipper a b = IndexedListZipper
  { _idxZipL :: [a]
  , _idxZipR :: [a]
  , _idxZipAnno :: b
  }

data TapeCell     = CellValid   | EndOfTape
data CellPresence = CellPresent | CellNotPresent

-- A monad defining operations over some 1-dimensional index, super generic.
class Monad m => MonadTape m where
    tapeRead  :: m (Maybe Int)
    tapeWrite :: Int -> m CellPresence
    tapeJump  :: Int -> m CellPresence
    tapeNext  :: m TapeCell
    tapePrev  :: m TapeCell

newtype AppM a = AppM
  { runTapeMachine :: State (IndexedListZipper Int Int) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (IndexedListZipper Int Int)
    )

instance MonadTape AppM where
    tapeRead = return Nothing
    tapeWrite _ = return CellNotPresent
    tapeJump _ = return CellNotPresent
    tapeNext = return EndOfTape
    tapePrev = return EndOfTape

d2Prog :: MonadTape m => m ()
d2Prog = do
    _ <- tapeNext
    _ <- tapeWrite 12
    _ <- tapeNext
    _ <- tapeWrite 12
    return ()

{-
-- A monad defining operations over some 1-dimensional index, super generic.
class Monad m => MonadTape' m where
    type Symbol m :: *
    type    Key m :: *
    tapeRead :: m (Maybe (Symbol m))
    tapeJump :: Key m -> m CellPresence
    tapeNext :: m TapeCell
    tapePrev :: m TapeCell

type Opcode = Int

-- Thread with state 'a'.
type Thread a = IndexedListZipper Int a
-}
