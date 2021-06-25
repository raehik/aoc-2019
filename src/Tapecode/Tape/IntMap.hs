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

instance Num b => Tape (IdxIntMap a b) where
    type Symbol   (IdxIntMap a b) = b
    type Index    (IdxIntMap a b) = Int
    type TapeAnno (IdxIntMap a b) = a
    tapeDefSym = const 0
    tapeNext a@(IdxIntMap m ptr len anno) =
        if   ptr' < len
        then Just (IdxIntMap m                                     ptr' len     anno)
        else Just (IdxIntMap (IntMap.insert ptr' (tapeDefSym a) m) ptr' (len+1) anno)
      where ptr' = ptr + 1
    tapePrev (IdxIntMap m ptr len anno) =
        if   ptr' >= 0
        then Just (IdxIntMap m ptr' len anno)
        else Nothing
      where ptr' = ptr - 1
    tapeRead a@(IdxIntMap m ptr _ _) =
        case IntMap.lookup ptr m of
          Nothing -> tapeDefSym a
          Just x  -> x
    tapeWrite a x@(IdxIntMap m ptr _ _) =
        set iimMap (IntMap.insert ptr a m) x
    tapeJump jmp a@(IdxIntMap m _ len anno) =
        if   jmp < 0
        then Nothing
        else if   jmp > len
             then Just (IdxIntMap (IntMap.insert jmp (tapeDefSym a) m) jmp (jmp+1) anno)
             else Just (IdxIntMap m jmp len anno)
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
