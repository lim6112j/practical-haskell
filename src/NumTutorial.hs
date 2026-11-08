module NumTutorial(complexFunc, main) where

data Complex = C Double Double deriving (Show, Eq)

instance Num Complex  where
  (C a b) + (C c d) = C (a + c) (b + d)
  (C a b) * (C c d) = C (a * c - b * d) (a * d + b * c)
  (C a b) - (C c d) = C (a - c) (b - d)
  negate (C a b) = C (-a) (-b)
  abs (C a b) = C (sqrt (a^2 + b^2)) 0
  fromInteger n = C (fromInteger n) 0
  signum c@(C a b) = let C n _ = abs c in C (a/n) (b/n)

complexFunc :: IO ()
complexFunc = do
  print $ C 1 2 + C 3 4
  print $ C 1 2 * C 3 4
  print $ C 1 2 - C 3 4
  print $ negate (C 1 2)
  print $ abs (C 1 2)
  print  ( 1 :: Complex)
  print $ signum (C 1 2)

main :: IO ()
main = complexFunc
