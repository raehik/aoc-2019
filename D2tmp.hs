{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc2019.D2 where

import           Control.Monad.State.Lazy
import           Data.Foldable (foldl')

-- List zipper over a '[a]' with single annotation 'b'.
data IndexedListZipper a b = IndexedListZipper
  { _idxZipL :: [a]
  , _idxZipR :: [a]
  , _idxZipAnno :: b
  }

type Tape = [Int]
type TapeState = IndexedListZipper Int Int

newtype TapeProgram a = TapeProgram
  { runTapeProgram :: State TapeState a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState TapeState
             )

data D2Error
  = D2EEditProgramFailed
    deriving (Eq, Ord, Show)

replaceTapeIndex :: Int -> a -> [a] -> Maybe [a]
replaceTapeIndex tmpn tmpa tmpl = go tmpa [] tmpn tmpl
  where
    go _ _      _ []     = Nothing
    go a behind 0 (_:xs) = Just $ foldl' (flip (:)) behind (a:xs)
    go a behind n (x:xs) = go a (x:behind) (n-1) xs

d2 :: Tape -> Either D2Error Int
d2 tape = do
    tape' <- preRunEdit tape
    --runTapeProgram program
    Right 0
  where
    preRunEdit t1 =
        case replaceTapeIndex 1 12 t1 of
          Nothing -> Left D2EEditProgramFailed
          Just t2 ->
            case replaceTapeIndex 2 2 t2 of
              Nothing -> Left D2EEditProgramFailed
              Just t3 -> Right t3

data ProgramState
  = StateIdle
  | StateHalt
  | StateError

execProgram :: TapeProgram ()
execProgram =
    state <- step
    case state of
      StateIdle  -> execProgram
      StateHalt  -> return ()
      StateError -> return () -- TODO
  where
    step :: TapeProgram ProgramState
    (x:xs) <- get
    handleOpcode x

handleOpcode :: Int -> TapeProgram ()
handleOpcode = \case
  99 -> return ()
   1 -> do
    tapeNext
    v1 <- getValAtPosAndIncrement
    v2 <- getValAtPosAndIncrement
    out <- writeAtPosAndIncrement (v1+v2)

getValAtPosAndIncrement :: TapeProgram Int
getValAtPosAndIncrement = do
    state <- get 

tapeNext :: TapeProgram ()
tapeNext = modify $ IndexedListZipper l (r:rs) a -> IndexedListZipper (r:l) rs a
