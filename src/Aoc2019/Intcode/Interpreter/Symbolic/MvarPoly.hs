module Aoc2019.Intcode.Interpreter.Symbolic.MvarPoly where

import           Prelude hiding (read)

import           Aoc2019.Utils
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder

-- TODO: pull MvarPoly into its own module, and give it a typevar (which will
-- normally be constrained to Num, concretely Int and Double)

type Var = Text

-- | a = term coefficient, b = power
newtype MvarPoly a b = MvarPoly { unMvarPoly :: Map (Map Var b) a }
  deriving (Eq, Ord, Show)

-- | Multiply two numeric 'MvarPoly's together. Does not attempt to retain
--   normal form.
mvarPolyMul :: (Ord b, Num a, Num b) => MvarPoly a b -> MvarPoly a b -> MvarPoly a b
mvarPolyMul (MvarPoly mL) (MvarPoly mR) = MvarPoly $ Map.foldrWithKey go Map.empty mR
  where
    --go :: (Num a, Num b) => Map Var b -> a -> Map (Map Var b) a -> Map (Map Var b) a
    go polyn coeff mComb = Map.foldrWithKey (goInner polyn coeff) mComb mL
    --goInner :: (Num a, Num b) => Map Var b -> a -> Map Var b -> a -> Map (Map Var b) a -> Map (Map Var b) a
    goInner polynL coeffL polynR coeffR mComb =
        let inner = Map.unionWith (+) polynL polynR
            coeff = coeffL * coeffR
         in Map.insertWith (+) inner coeff mComb

-- | Add two 'MvarPoly's together. Does not attempt to retain normal form.
mvarPolyAdd :: (Ord b, Num a, Num b) => MvarPoly a b -> MvarPoly a b -> MvarPoly a b
mvarPolyAdd (MvarPoly mL) (MvarPoly mR) = MvarPoly $ Map.unionWith (+) mL mR

data Sign
  = SignPos
  | SignNeg

pprintSign :: Sign -> Text
pprintSign = \case
  SignPos -> "+"
  SignNeg -> "-"

-- Takes an MvarPoly in normal form (no 0 coeffs, no 0 powers). Will print
-- "superfluous" terms if not in normal form. (Does handle empty expressions,
-- however, since we need to for good sign behaviour anyway.)
mvarPolyExpr :: (Ord a, Num a, Eq b, Num b, Show a, Show b) => MvarPoly a b -> Maybe Text
mvarPolyExpr (MvarPoly m) =
    (fmap toLazyText . Map.foldrWithKey buildExpr Nothing) m
  where
    --buildExpr :: Map Var Int -> Int -> Maybe Builder -> Maybe Builder
    buildExpr polynMap coeff = \case
      Nothing  ->
        case sign of
          SignPos -> Just term
          SignNeg -> Just $ fromLazyText (pprintSign sign) <> term
      Just bld -> Just $ bld <> " " <> fromLazyText (pprintSign sign) <> " " <> term
      where
        polyn = (Map.foldrWithKey buildPolyPart mempty) polynMap
        sign  = if coeff < 0 then SignNeg else SignPos
        term  = if coeff == 1 then polyn else fromLazyText (tshow (abs coeff)) <> polyn
    --buildPolyPart :: Var -> Int -> Builder -> Builder
    buildPolyPart var pwr bld
      | pwr == 1  = bld <> fromLazyText var
      | otherwise = bld <> fromLazyText var <> "^" <> fromLazyText (tshow pwr)
