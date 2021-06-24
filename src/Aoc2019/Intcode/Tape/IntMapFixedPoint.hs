{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Aoc2019.Intcode.Tape.IntMapFixedPoint where

import           Aoc2019.Intcode.Tape

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Control.Lens hiding (Index)
import           Data.Maybe (fromJust)

data IdxIntMapFP a = IdxIntMapFP
  { _iimfpMap :: IntMap (Either (IdxIntMapFP a) a)
  , _iimfpPtr :: Int
  , _iimfpLen :: Int
  } deriving (Eq, Ord, Show)
$(makeLenses ''IdxIntMapFP)

instance Tape (IdxIntMapFP a) where
    type Symbol (IdxIntMapFP a) = Either (IdxIntMapFP a) a
    type Index (IdxIntMapFP a) = Int
    tapeNext (IdxIntMapFP m ptr len) =
        if   ptr' <= len
        then Just (IdxIntMapFP m ptr' len)
        else Nothing
      where ptr' = ptr + 1
    tapePrev (IdxIntMapFP m ptr len) =
        if   ptr' >= 0
        then Just (IdxIntMapFP m ptr' len)
        else Nothing
      where ptr' = ptr - 1
    tapeRead (IdxIntMapFP m ptr _) = fromJust (IntMap.lookup ptr m)
    tapeWrite a x@(IdxIntMapFP m ptr _) = set iimfpMap (IntMap.insert ptr a m) x
    tapeJump jmp (IdxIntMapFP m _ len) =
        if   jmp <= len && jmp >= 0
        then Just (IdxIntMapFP m jmp len)
        else Nothing
    tapePos = view iimfpPtr

fromListToIdxIntMapFP :: [a] -> IdxIntMapFP a
fromListToIdxIntMapFP = go 0 IntMap.empty
  where
    go len im []     = IdxIntMapFP im 0 len
    go len im (x:xs) = go (len+1) (IntMap.insert len (Right x) im) xs

-- | Extract just the tape from an 'IdxIntMapFP'. Ignores branches.
idxIntMapFPTape :: IdxIntMapFP a -> [a]
idxIntMapFPTape iim =
    let m = view iimfpMap iim
        tape = IntMap.elems m
     in f tape
  where
    f :: [Either a b] -> [b]
    f [] = []
    f (x:xs) =
        case x of
          Left _ -> f xs
          Right x' -> x' : f xs
