module ListPredicate where

import qualified Data.List as DL

bothFilters :: (a -> Bool) -> [a] -> ([a], [a])
bothFilters p list = (filter p list, filter (not . p) list)
main :: IO ()
main = do
  print $ DL.partition (> (0 :: Integer)) [1, 2, -3, -4, -5, -6]
  print $ DL.find (< (-3 :: Integer)) [1, 2, -3, -4, -5, -6]
  let list = ["hello", "send", "stop", "receive"]
  print $ takeWhile (/= "stop") list
  print $ span (/= "stop") list
  let p x y = (even x && even y) || (odd x && odd y)
  print $ DL.nubBy p [1, 2, 3, 4, 5]
