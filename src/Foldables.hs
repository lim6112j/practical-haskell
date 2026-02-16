module Foldables(foldFunc) where
import Prelude hiding (foldr, foldl)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f initial [] = initial
foldr f initial (x:xs) = f x (foldr f initial xs)

data InfNumber a = MinusInfinity
                | Number a
                | PlusInfinity
                deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity x = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ initial [] = initial
foldl f initial (x:xs) = foldl f (f initial x) xs

foldFunc :: IO ()
foldFunc = do
    print $ foldr infMax MinusInfinity $ map Number [1,2,3]
    print $ foldr (\x y -> infMax (Number x) y) MinusInfinity [1,2,3]
    print $ foldl (\x y -> infMax x (Number y)) MinusInfinity [1,2,3]



