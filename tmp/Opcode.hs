module Aoc2019.Intcode.Opcode where

data ParamMode
  = PosMode
  | ImmMode
  | RelMode
    deriving (Eq, Show) -- TODO will only add an Ord instance if/when needed

-- | Returns Nothing for invalid instructions.
parseInst :: Integral a => a -> Maybe (Opcode, [ParamMode])
parseInst inst =
    case digits' inst of
      []               -> Nothing
      [opc]            -> 

      (opc1:opc2:opcs) -> Just (, _ parseParamModes opcs)
  where
    parseParamModes :: [Int]

-- | Retrieve a list of digits from an 'Integral', least significant first, and
--   group the two least significant digits together.
digits' :: Integral a => a -> [a]
digits' 0 = []
digits' x = x `mod` 100 : digits (x `div` 100)

-- | Retrieve a list of digits from an 'Integral', least significant first.
digits :: Integral a => a -> [a]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

