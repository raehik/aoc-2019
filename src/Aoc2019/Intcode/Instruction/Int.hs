-- | TODO
--   * consider disallowing forbidden parammodes in certain positions in the
--     type
--   * alternatively, checking at decode time

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Aoc2019.Intcode.Instruction.Int
  ( Error(..)
  , Instruction(..)
  , ParamMode(..)
  , decode
  ) where

data Error
  = ErrUnrecognizedOpcode
  | ErrTooManyParamModes
  | ErrUnrecognizedParamMode
    deriving (Eq, Show)

-- | Instructions parameterized over argument representations.
--
-- Inspired by glguy's Intcode interpreter.
data Instruction a
  = Add a a a
  | Mul a a a
  | In  a
  | Out a
  | Jnz a a
  | Jz  a a
  | Lt  a a a
  | Eq  a a a
  | Arb a
  | Hlt
    deriving (Eq, Functor, Foldable, Traversable, Show)

data ParamMode
  = PosMode
  | ImmMode
  | RelMode
    deriving (Eq, Show) -- TODO will only add an Ord instance if/when needed

defaultParamMode :: ParamMode
defaultParamMode = PosMode

decode' :: Int {- ^ opcode -} -> Maybe (Instruction ParamMode)
decode' n =
  case n `rem` 100 of
    1  -> fill (Add 1 2 3)
    2  -> fill (Mul 1 2 3)
    3  -> fill (In  1    )
    4  -> fill (Out 1    )
    5  -> fill (Jnz 1 2  )
    6  -> fill (Jz  1 2  )
    7  -> fill (Lt  1 2 3)
    8  -> fill (Eq  1 2 3)
    9  -> fill (Arb 1    )
    99 -> fill Hlt
    _  -> Nothing
  where
    fill = traverse (parameter n)

-- | Compute the parameter mode for an argument at a given position.
parameter :: Int -> Int -> Maybe ParamMode
parameter n i = paramMode (digit (i+1) n)

paramMode :: Int -> Maybe ParamMode
paramMode = \case
  0 -> Just PosMode
  1 -> Just ImmMode
  2 -> Just RelMode
  _ -> Nothing

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Int {- ^ position -} -> Int {- ^ number -} -> Int {- ^ digit -}
digit i x = x `quot` (10^i) `rem` 10

-- TODO
encode :: Integral a => Instruction ParamMode -> a
encode = const 0

-- | Retrieve a list of digits from an 'Integral', least significant first, and
--   group the two least significant digits together.
digits' :: Integral a => a -> [a]
digits' 0 = []
digits' x = x `mod` 100 : digits (x `div` 100)

-- | Retrieve a list of digits from an 'Integral', least significant first.
digits :: Integral a => a -> [a]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

-- | Decode an instruction to the opcode and parameter modes.
--
-- Rewritten from glguy's decoder -- I love how he stores and decodes
-- instructions, but he doesn't handle instructions with too many param modes,
-- and Traversable doesn't seem to be able to (I need a stateful traverse, that
-- rebuilds a structure _and_ produces a final state as well. I need a
-- simultaenous traverse and fold.) This function has no such issue.
decode :: Int {- ^ opcode -} -> Either Error (Instruction ParamMode)
decode n =
    case digits' n of
      [] {- n==0 -} -> Left ErrUnrecognizedOpcode
      opcode:params ->
        case opcode of
          1  -> fill3 Add params
          2  -> fill3 Mul params
          3  -> fill1 In  params
          4  -> fill1 Out params
          5  -> fill2 Jnz params
          6  -> fill2 Jz  params
          7  -> fill3 Lt  params
          8  -> fill3 Eq  params
          9  -> fill1 Arb params
          99 -> fill0 Hlt params
          _  -> Left ErrUnrecognizedOpcode
      where
        fillHelper f inst = \case
          []   -> f (inst defaultParamMode) []
          p:ps ->
            case paramMode p of
              Nothing -> Left ErrUnrecognizedParamMode
              Just p' -> f (inst p') ps
        fill0 inst = \case
          []  -> Right inst
          _:_ -> Left ErrTooManyParamModes
        fill1 = fillHelper fill0
        fill2 = fillHelper fill1
        fill3 = fillHelper fill2

{-
class HasIntcodeRepr a where
    encode :: a -> Int
    decode :: Int -> a

instance HasIntcodeRepr Int where
    encode = id
    decode = id
instance HasIntcodeRepr (Instruction ParamMode) where
    encode = encode
    decode = encode
-}
