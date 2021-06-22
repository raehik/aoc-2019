{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Aoc2019.Intcode.Tape where

import           Data.Kind

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Control.Lens hiding (Index)
import           Data.Maybe (fromJust)

class Tape t where
    type Symbol t :: Type
    type Index t  :: Type
    tapeNext  :: t -> Maybe t
    tapePrev  :: t -> Maybe t
    tapeRead  :: t -> Symbol t
    tapeWrite :: Symbol t -> t -> t
    tapeJump  :: Index t -> t -> Maybe t
    tapePos   :: t -> Index t

data IdxIntMap a = IdxIntMap
  { _iimMap :: IntMap a
  , _iimPtr :: Int
  , _iimLen :: Int
  } deriving (Eq, Ord, Show)
$(makeLenses ''IdxIntMap)

fromListToIdxIntMap :: [a] -> IdxIntMap a
fromListToIdxIntMap = go 0 IntMap.empty
  where
    go len im []     = IdxIntMap im 0 len
    go len im (x:xs) = go (len+1) (IntMap.insert len x im) xs

-- | Extract just the tape from an 'IdxIntMap'.
idxIntMapTape :: IdxIntMap a -> [a]
idxIntMapTape = map (\(_, v) -> v) . IntMap.toList . view iimMap

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

--------------------------------------------------------------------------------

{-

-- List zipper over a '[a]'.
data ListZipper a = ListZipper
  { _lzL :: [a]
  , _lzR :: [a]
  }
$(makeLenses ''ListZipper)

-- Non-empty list zipper of 'a' with single annotation 'b'.
data IdxNEZipper a b = IdxNEZipper
  { _inezL    :: [a]
  , _inezX    :: a
  , _inezR    :: [a]
  , _inezAnno :: b
  }
$(makeLenses ''IdxNEZipper)

-}
