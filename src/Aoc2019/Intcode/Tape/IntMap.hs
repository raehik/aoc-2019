{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Aoc2019.Intcode.Tape.IntMap where

import           Aoc2019.Intcode.Tape

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Control.Lens hiding (Index)
import           Data.Maybe (fromJust)

data IdxIntMap a = IdxIntMap
  { _iimMap :: IntMap a
  , _iimPtr :: Int
  , _iimLen :: Int
  } deriving (Eq, Ord, Show)
$(makeLenses ''IdxIntMap)

instance Tape (IdxIntMap a) where
    type Symbol (IdxIntMap a) = a
    type Index (IdxIntMap a) = Int
    tapeNext (IdxIntMap m ptr len) =
        if   ptr' <= len
        then Just (IdxIntMap m ptr' len)
        else Nothing
      where ptr' = ptr + 1
    tapePrev (IdxIntMap m ptr len) =
        if   ptr' >= 0
        then Just (IdxIntMap m ptr' len)
        else Nothing
      where ptr' = ptr - 1
    tapeRead (IdxIntMap m ptr _) = fromJust (IntMap.lookup ptr m)
    tapeWrite a x@(IdxIntMap m ptr _) = set iimMap (IntMap.insert ptr a m) x
    tapeJump jmp (IdxIntMap m _ len) =
        if   jmp <= len && jmp >= 0
        then Just (IdxIntMap m jmp len)
        else Nothing
    tapePos = view iimPtr

fromListToIdxIntMap :: [a] -> IdxIntMap a
fromListToIdxIntMap = go 0 IntMap.empty
  where
    go len im []     = IdxIntMap im 0 len
    go len im (x:xs) = go (len+1) (IntMap.insert len x im) xs

-- | Extract just the tape from an 'IdxIntMap'.
idxIntMapTape :: IdxIntMap a -> [a]
idxIntMapTape = IntMap.elems . view iimMap
