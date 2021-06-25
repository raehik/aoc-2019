-- | DSL for writing Tapecode interpreters.
--
-- This is the tagless final approach that lets us encode a monadic
-- specification, to then execute using a supported concrete implementation
-- later. It lets us push the concrete selection right to the end, so we can
-- stay super polymorphic and write a bunch of reusable code.
--
-- For full Intcode, you'll usually want an state monad over IO (to handle IO
-- instructions).

{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Tapecode.Interpreter where

import           Prelude hiding (read)

import           Data.Kind
import           Tapecode.Tape

-- | A 'MonadInterp' instance is a monad with an associated 'Tape'. Tapes are
-- pure stateful-esque data structures with a consistent interface to allow
-- updates. Many 'MonadInterp' operations will map closely to 'Tape' operations
-- -- the difference will usually be in how errors are handled, which depends on
-- the underlying monad.
class (Monad m, Tape (InterpTape m)) => MonadInterp m where
    type InterpTape m :: Type
    next :: m ()
    prev :: m ()
    read :: m (Symbol (InterpTape m))
    write :: Symbol (InterpTape m) -> m ()
    jump :: Index (InterpTape m) -> m ()
    readPos :: m (Index (InterpTape m))
    moveLeftmost  :: m ()
    moveRightmost :: m ()
    fullTape :: m (InterpTape m)
