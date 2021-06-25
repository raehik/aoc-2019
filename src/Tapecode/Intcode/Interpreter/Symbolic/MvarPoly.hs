module Tapecode.Intcode.Interpreter.Symbolic.MvarPoly where

import           Prelude hiding (read)

import           Utils
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder

type Var = Text

-- | Multivariate polynomial over variable @v@, term coefficient @a@,
--   and power @b@.
--
-- A multivariate polynomial is an expression containing multiple variables of
-- various powers. e.g. @x^2 + 2xy - 2z + 1@. This type allows you to select
-- your variable type, along with the numeric types used for powers and
-- coefficients (letting you target Diophantine equations only, for example).
--
-- This is most certainly not all that efficient, but I'm stupid, thus care not.
-- It gets me what I want in a very simple and clean manner.
newtype MvarPoly v a b = MvarPoly { unMvarPoly :: Map (Map v b) a }
  deriving (Eq, Ord)

instance (Show v, Show a, Show b, Ord v, Ord a, Eq b, Num a, Num b) => Show (MvarPoly v a b) where
    show m =
        case mvarPolyExpr m of
          Nothing   -> "<empty expr>"
          Just expr -> "(" <> Text.unpack expr <> ")"

-- | Multiply two numeric 'MvarPoly's together. Does not attempt to retain
--   normal form.
mvarPolyMul :: (Ord v, Ord b, Num a, Num b) => MvarPoly v a b -> MvarPoly v a b -> MvarPoly v a b
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
mvarPolyAdd :: (Ord v, Ord b, Num a, Num b) => MvarPoly v a b -> MvarPoly v a b -> MvarPoly v a b
mvarPolyAdd (MvarPoly mL) (MvarPoly mR) = MvarPoly $ Map.unionWith (+) mL mR

-- Takes an MvarPoly in normal form (no 0 coeffs, no 0 powers). Will print
-- "superfluous" terms if not in normal form. (Does handle empty expressions,
-- however, since we need to for good sign behaviour anyway.)
-- TODO: not ideal for strings, text etc. because now it quotes them, haha...
mvarPolyExpr :: (Ord v, Ord a, Num a, Eq b, Num b, Show v, Show a, Show b) => MvarPoly v a b -> Maybe Text
mvarPolyExpr = mvarPolyExpr' tshow

mvarConstExpr :: a -> MvarPoly v a b
mvarConstExpr x = MvarPoly (Map.singleton Map.empty x)

-- TODO: due to coeff == 1 check, the plain val won't print if it's 1 lol
mvarPolyExpr'
    :: (Ord v, Ord a, Num a, Eq b, Num b, Show a, Show b)
    => (v -> Text) -> MvarPoly v a b -> Maybe Text
mvarPolyExpr' textify (MvarPoly m) =
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
      | pwr == 1  = bld <> fromLazyText (textify var)
      | otherwise = bld <> fromLazyText (textify var) <> "^" <> fromLazyText (tshow pwr)

data Sign
  = SignPos
  | SignNeg

pprintSign :: Sign -> Text
pprintSign = \case
  SignPos -> "+"
  SignNeg -> "-"
