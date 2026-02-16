module LazynessTimemachine(timeMachineFunc) where

data TimeMachine = TM { manufacturer :: String, year :: Integer } deriving (Show, Eq)

timeMachineFrom :: String -> Integer -> [TimeMachine]
timeMachineFrom m y = TM m y : timeMachineFrom m (y + 1)

timeMachineFunc :: IO ()
timeMachineFunc = do
    let tms = timeMachineFrom "DeLorean" 1950
    print (take 5 tms)

