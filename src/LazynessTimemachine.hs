{-# LANGUAGE BangPatterns #-}

module LazynessTimemachine (timeMachineFunc, main) where

data TimeMachine = TM {manufacturer :: String, year :: Integer} deriving (Show, Eq)

timeMachineFrom :: String -> Integer -> [TimeMachine]
timeMachineFrom m y = TM m y : timeMachineFrom m (y + 1)

timeMachineFunc :: IO ()
timeMachineFunc = do
    let tms = timeMachineFrom "DeLorean" 1950
    print (take 5 tms)
sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
  where
    sumYears' [] z = z
    sumYears' (TM _ !y : ys) z = let !s = z + y in sumYears' ys s
main :: IO ()
main = timeMachineFunc
