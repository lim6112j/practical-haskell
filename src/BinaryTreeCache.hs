module BinaryTreeCache where

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
  | Leaf3
  deriving (Show, Eq)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) 
  = case compare v v2 of
    LT -> Node3 v2 c2 (treeInsert3 v c l) r
    EQ -> Node3 v c l r
    GT -> Node3 v2 c2 l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) = 
  case compare v v2 of
    EQ -> Node3 v c2 l r
    LT -> let newLeft = treeInsert4 v c l
              newCache = c2 <> cached newLeft <> cached r
              in Node3 v2 newCache newLeft r
    GT -> let newRight = treeInsert4 v c r
              newCache = c2 <> cached l <> cached newRight
              in Node3 v2 newCache l newRight
    where
      cached :: Monoid c => BinaryTree3 v c -> c
      cached (Node3 _ c _ _) = c
      cached Leaf3 = mempty
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

newtype Min = Min Double deriving Show

instance Semigroup Min where
  Min a <> Min b = Min $ min a b

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend = (<>)

