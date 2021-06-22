module Aoc2019.Utils where

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

tshow :: Show a => a -> Text
tshow = Text.pack . show
