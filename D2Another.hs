{- TODO:
  * by using ([a], a, [a]) (== [a], NonEmpty a) for zipper, we can simplify many
    operations, but lose the ability to work on totally empty programs, and
    semantics for rightmost tape end change (for better or worse)
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Aoc2019.D2 where

import           Control.Monad.State.Lazy
import           Aoc2019.Parsers
import           Control.Lens

-- List zipper over a '[a]'.
data ListZipper a = ListZipper
  { _zipL :: [a]
  , _zipR :: [a]
  }
$(makeLenses ''ListZipper)

-- List zipper over a '[a]' with single annotation 'b'.
data IndexedListZipper a b = IndexedListZipper
  { _idxZipL :: [a]
  , _idxZipR :: [a]
  , _idxZipAnno :: b
  }
$(makeLenses ''IndexedListZipper)

data TapeState = TapeState
  { _tsZipper :: ListZipper Int
  , _tsPC     :: Int
  }
$(makeLenses ''TapeState)

newtype AppM a = AppM { runApp :: State TapeState a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState TapeState
    )

initZipper :: [a] -> ListZipper a
initZipper = ListZipper []

initTapeState :: ListZipper Int -> TapeState
initTapeState z = TapeState z 0

evalProgram :: AppM a -> [Int] -> a
evalProgram f prog = evalState (runApp f) (initTapeState (initZipper prog))

d2Prog :: AppM Int
d2Prog = do
    tapeZipLeftmost

solve :: IO (Maybe Int)
solve = parseAndUseResource "d2.txt" pIntcode (evalProgram d2Prog)

--------------------------------------------------------------------------------

tapeZipLeftmost :: AppM ()
tapeZipLeftmost = do
    modify $ \(TapeState z _) -> TapeState (zipLeftmost z) 0

--------------------------------------------------------------------------------

tapeReadUnsafe :: AppM Int
tapeReadUnsafe = do
    TapeState (ListZipper _ zr) _ <- get
    case zr of
      []  -> error "tried to access tape at empty cell"
      x:_ -> return x

tapeWriteUnsafe :: Int -> AppM ()
tapeWriteUnsafe x = do
    modify $ over (tsZipper . zipR) f
  where
    f []     = error "tried to access tape at empty cell"
    f (_:xs) = x:xs

zipLeftmost :: ListZipper a -> ListZipper a
zipLeftmost (ListZipper zl zr) =
    let zr' = consEach zl zr
     in ListZipper [] zr'

zipLeftmost' :: IndexedListZipper a b -> IndexedListZipper a b
zipLeftmost' (IndexedListZipper zl zr a) =
    let zr' = consEach zl zr
     in IndexedListZipper [] zr' a

splitOn :: forall a. Eq a => a -> [a] -> [[a]]
splitOn a = go [] []
  where
    go :: [[a]] -> [a] -> [a] -> [[a]]
    go sublists l@(_:_) []     = l:sublists
    go sublists []      []     = sublists
    go sublists l     (x:xs)
      | a == x    = go (l:sublists) [] xs
      | otherwise = go sublists (x:l) xs

-- adapted from xmonad-contrib
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l = f : split e (rest ls)
  where
    (f, ls) = span (/=e) l
    rest s
      | s == []   = []
      | otherwise = tail s

-- | Cons each element of 'l' onto 'r'.
-- TODO: == foldr (:) r l?
consEach :: [a] -> [a] -> [a]
consEach []    r = r
consEach (x:l) r = consEach l (x:r)
