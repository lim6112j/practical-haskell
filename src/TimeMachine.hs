module TimeMachine where

data TimeMachine = TM {manufacturer :: String, year :: Integer} deriving (Eq, Show)
timeMachineFrom :: String -> Integer -> [TimeMachine]
timeMachineFrom mf y = TM mf y : timeMachineFrom mf (y + 1)
timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachineFrom "Timely Inc." 100

main :: IO ()
main = do
  print $ take 3 timelyIncMachines
