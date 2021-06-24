module Aoc2019.Intcode.Instruction.IntSpec where

import Test.Hspec
import Aoc2019.Intcode.Instruction.Int

spec :: Spec
spec = do
    describe "Instructions" $ do
      it "decodes an ADD with no parameter modes (all defaults)" $ do
        decode 1 `shouldBe` Right (Add dpm dpm dpm)
      it "decodes an ADD with partial parameter modes" $ do
        decode 101 `shouldBe` Right (Add ImmMode dpm dpm)
      it "decodes an ADD with full parameter modes" $ do
        decode 21001 `shouldBe` Right (Add PosMode ImmMode RelMode)
      it "fails to decode an ADD with too many parameter modes" $ do
        -- Note: Trying something like 021001 here will pass successfully.
        -- Intcode blurs the line between integer and string, enough that it
        -- appears more fun to stick with integers throughout. And in integer
        -- representation, leading zeroes are ignored.
        decode 121001 `shouldBe` Left ErrTooManyParamModes
      it "fails to decode an ADD with an invalid parameter mode" $ do
        decode 301 `shouldBe` Left ErrUnrecognizedParamMode
      it "decodes a HALT" $ do
        decode 99 `shouldBe` Right Hlt
      it "fails to decode a HALT with parameter modes" $ do
        decode 199 `shouldBe` Left ErrTooManyParamModes
      it "fails to decode an unsupported opcode" $ do
        decode 10 `shouldBe` Left ErrUnrecognizedOpcode

-- | Default 'ParamMode'.
dpm :: ParamMode
dpm = defaultParamMode
