module Intcode.Interpreter.Symbolic.MvarPolySpec where

import           Test.Hspec
import           Intcode.Interpreter.Symbolic.MvarPoly

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)

type MvarPoly' = MvarPoly Text Int Int

-- TODO: Place in MvarPoly module.
mvp :: (Ord v, Ord b) => [([(v, b)], a)] -> MvarPoly v a b
mvp = MvarPoly . Map.fromList . map (mapFirst Map.fromList)
  where
    mapFirst :: (a -> a') -> (a, b) -> (a', b)
    mapFirst f (x, y) = (f x, y)

spec :: Spec
spec = do
    describe "MvarPoly" $ do
      it "addition: x + 1" $ do
        let e1 = mvp [([("x", 1)], 1)]
            e2 = mvp [([],  1)]
            e  = mvp [([("x", 1)], 1), ([], 1)]
        mvarPolyAdd e1 e2 `shouldBe` e
      it "addition: (x + y) + 1" $ do
        let e1 = mvp [([("x", 1)], 1), ([("y", 1)], 1)]
            e2 = mvp [([],  1)]
            e  = mvp [([("x", 1)], 1), ([("y", 1)], 1), ([], 1)]
        mvarPolyAdd e1 e2 `shouldBe` e
