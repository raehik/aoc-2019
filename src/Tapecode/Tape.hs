{-# LANGUAGE TypeFamilies #-}

module Tapecode.Tape where

import           Data.Kind
--import           Control.Lens (ALens')

class Tape t where
    type Symbol   t :: Type
    type Index    t :: Type
    type TapeAnno t :: Type
    tapeNext  :: t -> Maybe t
    tapePrev  :: t -> Maybe t
    tapeRead  :: t -> Symbol t
    tapeWrite :: Symbol t -> t -> t
    tapeJump  :: Index t -> t -> Maybe t
    tapePos   :: t -> Index t
    --tapeAnno  :: ALens' t (TapeAnno t)
    tapeAnnoGet :: t -> TapeAnno t
    tapeAnnoSet :: TapeAnno t -> t -> t
