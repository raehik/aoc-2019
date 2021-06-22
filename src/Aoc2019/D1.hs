module Aoc2019.D1 where

type Mass = Int
type Fuel = Int

-- fuel = floor(mass/3) - 2
fuelRequiredShallow :: Mass -> Fuel
fuelRequiredShallow mass = (mass `div` 3) - 2

-- fuel = floor(mass/3) - 2 (recursively until <=0)
fuelRequiredDeep :: Mass -> Fuel
fuelRequiredDeep m = go 0 (fuelRequiredShallow m)
  where
    go :: Fuel -> Mass -> Fuel
    go totalFuel mass
      | mass <= 0 = totalFuel
      | otherwise = go (totalFuel+mass) (fuelRequiredShallow mass)

-- | = 3352674
p1Solution :: IO Int
p1Solution = do
    strIn <- readFile "res/d1.txt"
    let moduleMasses = map read (lines strIn)
    return $ sum (map fuelRequiredShallow moduleMasses)

-- | = 5026151
p2Solution :: IO Int
p2Solution = do
    strIn <- readFile "res/d1.txt"
    let moduleMasses = map read (lines strIn)
    return $ sum (map fuelRequiredDeep moduleMasses)
