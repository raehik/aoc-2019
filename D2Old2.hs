{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.D2 where

import           Control.Monad.State.Lazy

type Opcode = Int

-- List zipper over a '[a]' with single annotation 'b'.
data IndexedListZipper a b = IndexedListZipper
  { _idxZipL :: [a]
  , _idxZipR :: [a]
  , _idxZipAnno :: b
  }

-- Thread with state 'a'.
type Thread a = IndexedListZipper Opcode a

{-
class MonadTape c m | m -> c where
    tapeRead :: m c
    tapeWrite :: c -> m ()
    tapeLeft :: m Bool
    tapeRight :: m Bool
-}

data TapeCell     = CellValid   | EndOfTape
data CellPresence = CellPresent | CellNotPresent

-- "Tape" (1-dimensional index).
class Monad m => MonadTape m where
    type Symbol m :: *
    type    Key m :: *
    tapeRead :: m (Maybe (Symbol m))
    tapeJump :: Key m -> m CellPresence
    tapeNext :: m TapeCell
    tapePrev :: m TapeCell

newtype TapeMachine sym key anno = TapeMachine
  { runTapeMachine :: State (IndexedListZipper sym anno) () }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (IndexedListZipper sym anno)
    )

instance MonadTape (TapeMachine sym key anno) where
    type Symbol (TapeMachine sym key anno) = sym
    type    Key (TapeMachine sym key anno) = key
    tapeRead = Nothing
    tapeJump _ = CellNotPresent
    tapeNext = EndOfTape
    tapePrev = EndOfTape
