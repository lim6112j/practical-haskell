module TreeTutorial(treeFunc, main) where

data TravelGuide = TravelGuide {
    title :: String,
    authors :: [String],
    price :: Double
} deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1 
                 | Leaf1 
                 deriving Show
treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 _ Leaf1 = Nothing
treeFind1 guide (Node1 guide' left right) = 
    case compare guide guide' of
        LT -> treeFind1 guide left
        EQ -> Just guide'
        GT -> treeFind1 guide right

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 guide Leaf1 = Node1 guide Leaf1 Leaf1
treeInsert1 guide (Node1 guide' left right) = 
    case compare guide guide' of
        LT -> Node1 guide' (treeInsert1 guide left) right
        EQ -> Node1 guide' left right
        GT -> Node1 guide' left (treeInsert1 guide right)

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                  | Leaf2
                  deriving (Show, Eq, Ord)

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 _ Leaf2 = Nothing
treeFind2 guide (Node2 guide' left right) = 
    case compare guide guide' of
        LT -> treeFind2 guide left
        EQ -> Just guide'
        GT -> treeFind2 guide right

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2) && (t1 < t2 || (t1 == t2 && a1 <= a2)) 

treeFunc :: IO ()
treeFunc = do
    let guide1 = TravelGuide "Paris" ["John"] 25.0
    let guide2 = TravelGuide "Rome" ["Jane"] 30.0
    let guide3 = TravelGuide "London" ["Bob"] 20.0
    let tree = treeInsert1 guide1 (treeInsert1 guide2 (treeInsert1 guide3 Leaf1))
    print tree
    print $ treeFind1 guide1 tree
    print $ treeFind1 guide2 tree
    print $ treeFind1 guide3 tree
    let tg1 = TGByPrice guide1
    let tg2 = TGByPrice guide2
    let tg3 = TGByPrice guide3
    print $ tg1 <= tg2
    print $ tg2 <= tg3
    print $ tg3 <= tg1

main :: IO ()
main = treeFunc
