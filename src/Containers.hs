module Containers () where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree

result :: (M.Map String Int, M.Map String Int, M.Map String Int, M.Map String Int)
result =
    let m1 = M.singleton "hello" 3
        m2 = M.insert "bye" 2 m1
        m3 = M.insert "hello" 5 m2
        m4 = M.insertWith (+) "hello" 7 m3
     in (m1, m2, m3, m4)
m :: M.Map String Integer
m = M.fromList [("hello", 1), ("bye", 2)]

setExample =
    let s1 = S.insert "welcome" $ S.singleton "hello"
        s2 = S.fromList ["hello", "bye"]
     in (s1 `S.intersection` s2, "welcome" `S.member` s2, S.map length s2)

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
    let subtreesTraversed = concat $ map (preOrder f) subtrees
     in f v : subtreesTraversed
pictureTree :: Tree Int
pictureTree = Node 1 [Node 2 [Node 3 [], Node 4 [], Node 5 []], Node 6 []]

main :: IO ()
main = do
    print result
    print (M.map (* 2) m, M.foldr (+) 0 m)
    print $ S.insert "welcome" $ S.singleton "hello"
    let ss = S.fromList ["hello", "bye", "hello"]
    print ss
    print $ S.toList ss
    print setExample
    print $ preOrder show pictureTree
    print $ flatten pictureTree
