module Foldables (foldFunc, main) where

import Prelude hiding (foldl, foldr)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ initial [] = initial
foldr f initial (x : xs) = f x (foldr f initial xs)

data InfNumber a
    = MinusInfinity
    | Number a
    | PlusInfinity
    deriving (Show)

infMax :: (Ord a) => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity x = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

sumInfNumber :: (Num a) => InfNumber a -> InfNumber a -> InfNumber a
sumInfNumber (Number a) (Number b) = Number (a + b)
sumInfNumber PlusInfinity _ = PlusInfinity
sumInfNumber _ PlusInfinity = PlusInfinity
sumInfNumber MinusInfinity _ = MinusInfinity
sumInfNumber _ MinusInfinity = MinusInfinity

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ initial [] = initial
foldl f initial (x : xs) = foldl f (f initial x) xs

foldFunc :: IO ()
foldFunc = do
    print $ foldr sumInfNumber (Number 0) $ map Number [1, 2, 3]
    print $ foldr infMax MinusInfinity $ map Number [1, 2, 3]
    print $ foldr (\x y -> infMax (Number x) y) MinusInfinity [1, 2, 3]
    print $ foldl (\x y -> infMax x (Number y)) MinusInfinity [1, 2, 3]

main :: IO ()
main = foldFunc
