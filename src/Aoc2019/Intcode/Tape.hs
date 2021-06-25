{-# LANGUAGE TypeFamilies    #-}

module Aoc2019.Intcode.Tape where

import           Data.Kind

class Tape t where
    type Symbol t :: Type
    type Index t  :: Type
    tapeNext  :: t -> Maybe t
    tapePrev  :: t -> Maybe t
    tapeRead  :: t -> Symbol t
    tapeWrite :: Symbol t -> t -> t
    tapeJump  :: Index t -> t -> Maybe t
    tapePos   :: t -> Index t

    -- TODO testing...
    tapeFull  :: t -> t
