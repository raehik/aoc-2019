-- | Wrapper module for instruction type.
--
-- We can easily handle instructions purely and away from the program flow,
-- which also gives us the flexibility to consider how we model them. That is,
-- should we be handling 'Int's, or 'String'-likes? Both approaches work, and
-- I'd be curious to know the overall space & time differences between them.

module Aoc2019.Intcode.Instruction
  ( module Aoc2019.Intcode.Instruction.Int
  ) where

import           Aoc2019.Intcode.Instruction.Int
