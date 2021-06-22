{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.D2 where

import           Control.Monad.State.Lazy

{-
class MonadTape c m | m -> c where
    tapeRead :: m c
    tapeWrite :: c -> m ()
    tapeLeft :: m Bool
    tapeRight :: m Bool
-}

class MonadTape m where
    type Symbol m :: *
    tapeRead :: m (Symbol m)
    tapeWrite :: (Symbol m) -> m ()
    tapeLeft :: m ()
    tapeRight :: m ()

newtype TapeMachine c a = TapeMachine { runTapeMachine :: State (Tape c, Int) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Tape c, Int)
    )
newtype Tape a = Tape { tape :: ([a], [a]) } deriving (Eq, Ord, Show)

instance MonadTape (TapeMachine c) where
    type Symbol (TapeMachine c) = c
    tapeRead    = gets (tape . fst) >>= return . head . snd
    tapeWrite x = modify $ \(Tape tp, i) ->
        case snd tp of
          []   -> error "tried to write to empty cell"
          _:cs -> (Tape (fst tp, x:cs), i)
    tapeLeft  = do
        st <- get
        let tp = (tape . fst) st
        case zipLeft tp of
          Just tp' -> put (Tape tp', snd st) >> return ()
          Nothing  -> return ()
    tapeRight = do
        st <- get
        let tp = (tape . fst) st
        case zipRight tp of
          Just tp' -> put (Tape tp', snd st) >> return ()
          Nothing  -> return ()

zipLeft :: ([a], [a]) -> Maybe ([a], [a])
zipLeft (l:ls, rs) = Just (ls, l:rs)
zipLeft _          = Nothing

zipRight :: ([a], [a]) -> Maybe ([a], [a])
zipRight (ls, r:rs) = Just (r:ls, rs)
zipRight _          = Nothing

{-
instance MonadTape a (TapeMachine a) where
    tapeRead    = gets (tape . fst) >>= return . head . snd
    tapeWrite x = modify $ \(Tape tp, i) ->
        case snd tp of
          []   -> error "tried to write to empty cell"
          _:cs -> (Tape (fst tp, x:cs), i)
    tapeLeft  = do
        st <- get
        let tp = (tape . fst) st
        case zipLeft tp of
          Just tp' -> put (Tape tp', snd st) >> return True
          Nothing  -> return False
    tapeRight = do
        st <- get
        let tp = (tape . fst) st
        case zipRight tp of
          Just tp' -> put (Tape tp', snd st) >> return True
          Nothing  -> return False
-}

{-
exProgSwap :: Monad m => MonadTape Int m => m Int
exProgSwap = do
    c1 <- tapeRead
    _ <- tapeRight
    c2 <- tapeRead
    tapeWrite c1
    _ <- tapeLeft
    tapeWrite c2
    tapeRead

exTapeShortInc :: (Tape Int, Int)
exTapeShortInc = (Tape ([], [1,2,3,4,5]), 0)

exRunTape :: State (Tape a, Int) a -> (Tape a, Int) -> (a, (Tape a, Int))
exRunTape = runState
-}
