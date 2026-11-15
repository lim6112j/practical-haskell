{- |
Module      : Ex01FoldableTraversable
Description : 연습 1 — Foldable / Traversable / Monoid / Alternative

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  아래 TODO가 표시된 함수/인스턴스의 `undefined`를 직접 채워 넣으면 된다.

  실행:
      stack run ex01-foldable-traversable
-}
module Ex01FoldableTraversableAnswer (
  main,
) where

import Control.Applicative (Alternative (..))
import Data.Foldable (fold, toList)
import Data.Monoid (Product (..), Sum (..))

-- ---------------------------------------------------------------------------
-- 연습에 사용할 자료구조: 이진 트리 (재귀적, 균형 잡히지 않음)
-- ---------------------------------------------------------------------------

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

sampleTree :: Tree Int
sampleTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node (Node Leaf 3 Leaf) 4 Leaf)

-- ---------------------------------------------------------------------------
-- TODO 1. Functor 인스턴스
--
--   목표: 트리의 모든 원소에 함수를 적용한다.
--
--   힌트:
--     * Leaf 는 그대로.
--     * Node l x r 에는 x 에 f 를 적용하고, l 과 r 에는 재귀적으로 fmap.
--     * 그림으로 그려보면, "각 노드를 방문해서 값만 바꾼다" 이다.
-- ---------------------------------------------------------------------------

instance Functor Tree where
  fmap f (Node a x b) = Node (fmap f a) (f x) (fmap f b)
  fmap _ Leaf = Leaf

-- ---------------------------------------------------------------------------
-- TODO 2. Foldable 인스턴스
--
--   목표: 트리를 "왼쪽 → 현재 → 오른쪽" 순서로 한 줄로 펼친다.
--         (foldr / foldMap 이 이 순서를 따르도록)
--
--   힌트:
--     * foldMap 은 Monoid m 이 필요하다. 각 원소 x 를 `mappend (pure x)` 로
--       승격시키고, 세 조각을 (왼쪽) <> (현재) <> (오른쪽) 으로 합친다.
--       즉:  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r
--     * foldr 도 같은 순서로 재귀:
--         foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l
--       (오른쪽을 먼저 접어서, 왼쪽으로 내려가는 형태)
--     * foldMap 만 제대로 만들면 foldr/foldl/toList/length/null 등은
--       기본 구현이 자동으로 파생된다. 굳이 둘 다 만들 필요는 없지만,
--       둘 다 직접 만들어 보면 "Foldable = Monoid 로의 동형사상" 감각이 잡힌다.
--     * 왼쪽-현재-오른쪽 순서를 inorder 라고 부른다.
-- ---------------------------------------------------------------------------

instance Foldable Tree where
  foldMap f (Node a x b) = foldMap f a <> f x <> foldMap f b
  foldMap _ Leaf = mempty

  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l
  foldr _ z Leaf = z

-- ---------------------------------------------------------------------------
-- TODO 3. Traversable 인스턴스
--
--   목표: 트리를 "순회"하면서, 각 원소에 Applicative 효과를 씌운 새 트리를 만든다.
--         예: traverse (\x -> [x, -x]) sampleTree  -- 가능한 모든 부호 조합
--
--   힌트:
--     * traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
--       (Leaf 는 pure Leaf)
--     * traverse 와 sequenceA 는 서로를 기본 구현으로 쓸 수 있는데,
--       이렇게 "맞보게" 정의하면 무한 재귀에 빠진다.
--       최소한 하나는 직접 재귀 구현해야 한다.
--     * Functor/Foldable 인스턴스가 먼저 필요하다 (superclass).
--     * traverse 의 타입을 천천히 읽어봐라:
--         (a -> f b) -> t a -> f (t b)
--       "구조(t)는 보존되고, 안의 값만 효과(f)를 입는다" 가 핵심이다.
-- ---------------------------------------------------------------------------

instance Traversable Tree where
  traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r
  traverse _ Leaf = pure Leaf

-- ---------------------------------------------------------------------------
-- TODO 4. Monoid 를 이용한 합계 / 곱
--
--   목표: Monoid 의 항등원과 결합법칙만으로 트리를 요약한다.
--
--   힌트:
--     * `fold` 은 foldMap id 이다. 즉 구조 안에 "이미 Monoid 인 값" 이 있으면
--       그냥 fold 로 다 합쳐진다.
--     * getSum . fold . fmap Sum  -- 합계
--     * getProduct . fold . fmap Product -- 곱
--     * 직접 foldr 로도 써 보라. 둘이 왜 같은지 설명할 수 있으면 됐다.
-- ---------------------------------------------------------------------------

treeSum :: Tree Int -> Int
-- treeSum (Node l x r) = treeSum l + x + treeSum r
-- treeSum Leaf = 0
-- treeSum = getSum . fold . (fmap Sum)
treeSum = foldr (+) 0

treeProduct :: Tree Int -> Int
-- treeProduct (Node l x r) = treeProduct l * x * treeProduct r
-- treeProduct Leaf = 1
-- treeProduct = getProduct . fold . (fmap Product)
treeProduct = foldr (*) 1

-- ---------------------------------------------------------------------------
-- TODO 5. Alternative: "빈 값" 과 "선택"
--
--   배경:
--     class Applicative f => Alternative f where
--       empty :: f a
--       (<|>) :: f a -> f a -> f a
--     Maybe 에서: empty = Nothing, (<|>) = 첫 번째 Just.
--     [] 에서:    empty = [], (<|>) = (++). 즉 "모든 경우의 수" 를 뜻한다.
--     Maybe 는 "실패할 수 있는 계산" 이고, [] 는 "비결정론" 이다.
--     둘을 같은 인터페이스로 다룰 수 있다는 게 포인트.
--
--   목표: Alternative 가 필요한 모든 타입에서 동작하는 조합기들을 만들어라.
--
--   힌트:
--     * optional 은 "실패하면 Nothing" 으로 되돌린다. 힌트: `fmap` 과 `(<|>)`.
--     * choiceList 는 리스트의 "첫 성공" 을 고른다. foldr 로 (<|>) 을 접어라.
--       (Data.Foldable 의 asum 이 정확히 이 일을 한다 — 먼저 asum 으로 풀고,
--        나중에 직접 구현해 보라.)
-- ---------------------------------------------------------------------------

optional' :: (Alternative f) => f a -> f (Maybe a)
optional' = (<|> pure Nothing) . fmap Just

choiceList :: (Alternative f) => [f a] -> f a
choiceList = foldr (<|>) empty

-- ---------------------------------------------------------------------------
-- TODO 6 (선택). "Foldable 이 있으면 Monoid 가 된다"
--
--   목표: 임의의 Foldable t 와 Monoid a 에 대해 t a 의 합을 만드는 함수를
--         직접 써 보라.
--
--   힌트:
--     * foldMap 을 쓰면 한 줄이다.
--     * 이 함수가 Data.Foldable 의 `fold` 과 어떤 관계인지, 그리고
--       "Endo" (Data.Monoid) 를 쓰면 왜 강력한지도 찾아보라.
-- ---------------------------------------------------------------------------

concatAll :: (Foldable t, Monoid a) => t a -> a
concatAll = fold

-- ---------------------------------------------------------------------------
-- 메인: TODO 를 다 채우면 각 print 의 결과가 주석과 같아야 한다.
-- (주석의 기대값은 직접 계산해서 확인해 봐라 — TODO 2 의 순서가 핵심이다.)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex01: Foldable / Traversable / Monoid / Alternative ==="

  -- TODO 2 확인: inorder 순서
  print (toList sampleTree) -- [1,2,3,4]

  -- TODO 4
  print (treeSum sampleTree) -- 10
  print (treeProduct sampleTree) -- 24

  -- TODO 3: 가능한 모든 부호 조합의 수는 2^4 = 16, 각 트리의 합은 0
  let signed = traverse (\x -> [x, -x]) sampleTree
  print (length signed) -- 16
  print (map treeSum signed) -- [0,0,0,...] 길이 16

  -- TODO 5
  print (optional' (Just 3)) -- Just (Just 3)
  print (optional' Nothing :: Maybe (Maybe Int)) -- Just Nothing  (Applicative Maybe)
  print (choiceList [Nothing, Just 2, Just 3]) -- Just 2
  print (choiceList [[2], [3, 4]] :: [Int]) -- [2,3,4]

  -- TODO 6
  print (getSum (concatAll (fmap Sum sampleTree))) -- 10
  print (getSum (fold (fmap Sum sampleTree))) -- 10 (위와 같아야 함)
