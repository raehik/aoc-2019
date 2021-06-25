{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Tapecode.Tape.IntMap where

import           Tapecode.Tape

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Control.Lens hiding (Index)
import           Data.Maybe (fromJust)

data IdxIntMap a b = IdxIntMap
  { _iimMap  :: IntMap b
  , _iimPtr  :: Int
  , _iimLen  :: Int
  , _iimAnno :: a
  } deriving (Eq, Ord, Show)
$(makeLenses ''IdxIntMap)

instance Tape (IdxIntMap a b) where
    type Symbol   (IdxIntMap a b) = b
    type Index    (IdxIntMap a b) = Int
    type TapeAnno (IdxIntMap a b) = a
    tapeNext (IdxIntMap m ptr len anno) =
        if   ptr' <= len
        then Just (IdxIntMap m ptr' len anno)
        else Nothing
      where ptr' = ptr + 1
    tapePrev (IdxIntMap m ptr len anno) =
        if   ptr' >= 0
        then Just (IdxIntMap m ptr' len anno)
        else Nothing
      where ptr' = ptr - 1
    tapeRead (IdxIntMap m ptr _ _) = fromJust (IntMap.lookup ptr m)
    tapeWrite a x@(IdxIntMap m ptr _ anno) =
        set iimMap (IntMap.insert ptr a m) x
    tapeJump jmp (IdxIntMap m _ len anno) =
        if   jmp <= len && jmp >= 0
        then Just (IdxIntMap m jmp len anno)
        else Nothing
    tapePos = view iimPtr
    tapeAnnoGet = view iimAnno
    tapeAnnoSet = set iimAnno

fromListToIdxIntMap :: a -> [b] -> IdxIntMap a b
fromListToIdxIntMap anno = go 0 IntMap.empty
  where
    go len im []     = IdxIntMap im 0 len anno
    go len im (x:xs) = go (len+1) (IntMap.insert len x im) xs

-- | Extract just the tape from an 'IdxIntMap'.
idxIntMapTape :: IdxIntMap a b -> [b]
idxIntMapTape = IntMap.elems . view iimMap
