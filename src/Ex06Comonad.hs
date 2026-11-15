{- |
Module      : Ex06Comonad
Description : 연습 6 — Comonad: Monad 의 쌍대(dual)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수/인스턴스의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex06-comonad

  배경 (Monad 와의 대비):
    Monad    :  return/join    — "값을 문맥에 넣고, 문맥을 벗긴다"
    Comonad  :  extract/duplicate — "문맥에서 값을 꺼내고, 문맥을 확장한다"
    즉 화살표 방향이 반대다. 그래서 '듀얼(dual)' 이라고 부른다.

      class Functor w => Comonad w where
        extract   :: w a -> a                    -- 문맥에서 현재 값 꺼내기
        duplicate :: w a -> w (w a)              -- 문맥을 한 겹 더 씌우기
        extend    :: (w a -> b) -> w a -> w b    -- 모든 위치에서 같은 규칙 적용
        extend f  = fmap f . duplicate

    직관: Comonad 는 "주변을 볼 수 있는 값" 이다. 현재 위치 + 이웃들.
    셀룰러 오토마타, 이미지 처리, 스트림 윈도우, Zipper, Store(상점) 등이 전형적 예.
-}
module Ex06Comonad (
  main,
) where

{- | Comonad 클래스를 직접 정의한다(패키지 의존성을 늘리지 않기 위해).
  실제로는 `comonad` 패키지의 Control.Comonad 가 이것과 동등하다.
  (Data.Functor.Extend / Control.Comonad.Store 등도 그 패키지에 있다.)
-}
class (Functor w) => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  duplicate = extend id

-- ---------------------------------------------------------------------------
-- 예제 1. Stream — 끝없는 스트림
-- ---------------------------------------------------------------------------

data Stream a = Stream a (Stream a)
  deriving (Eq, Show)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Stream x xs) = x : sTake (n - 1) xs

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Stream x (sIterate f (f x))

-- ---------------------------------------------------------------------------
-- TODO 1. Stream 의 Functor / Comonad 인스턴스
--
--   목표:
--     * fmap 은 모든 원소에 적용.
--     * extract 는 "현재(head)" 값.
--     * duplicate 는 "모든 위치에서 시작하는 부분스트림들의 스트림" (= tails).
--       힌트:  duplicate s@(Stream _ xs) = Stream s (duplicate xs)
--       ^^^ 재귀를 빼먹으면 타입이 안 맞는다:
--       결과의 둘째 필드는 Stream (Stream a) 여야 하는데,
--       xs 는 Stream a 라서 그대로는 못 들어간다.
--       duplicate = tails (모든 위치의 접미사 스트림) 라는 의미를 떠올려라.
--       이 한 줄이 왜 성립하는지, 그리고 왜 게으름(laziness) 덕분에
--       무한한 값이 되는지 생각해 봐라.
--     * extend 는 duplicate 로부터 한 줄로 유도된다. (직접 재귀해도 좋다)
-- ---------------------------------------------------------------------------

instance Functor Stream where
  fmap f (Stream a s) = Stream (f a) (fmap f s)

instance Comonad Stream where
  extract (Stream a _) = a

  duplicate s@(Stream _ xs) = Stream s (duplicate xs)

  -- extend f s@(Stream _ xs) = Stream (f s) (extend f xs)
  extend f = fmap f . duplicate

-- ---------------------------------------------------------------------------
-- 예제 2. Zipper — "현재 위치" 가 있는 리스트
--   (왼쪽은 역순으로 저장, 오른쪽은 정순)
-- ---------------------------------------------------------------------------

data Zipper a = Zipper [a] a [a]
  deriving (Eq, Show)

zipRight :: Zipper a -> Maybe (Zipper a)
zipRight (Zipper l x (r : rs)) = Just (Zipper (x : l) r rs)
zipRight _ = Nothing

zipLeft :: Zipper a -> Maybe (Zipper a)
zipLeft (Zipper (l : ls) x r) = Just (Zipper ls l (x : r))
zipLeft _ = Nothing

toListZ :: Zipper a -> [a]
toListZ (Zipper l x r) = reverse l ++ [x] ++ r

-- ---------------------------------------------------------------------------
-- TODO 2. Zipper 의 Comonad 인스턴스
--
--   목표: duplicate 는 "모든 위치를 현재로 만든 Zipper 들의 Zipper".
--
--   힌트:
--     * 왼쪽/오른쪽 이웃 Zipper 를 만드는 보조 함수가 필요하다:
--         lefts  z@(Zipper (l:ls) _ r) = let z' = Zipper ls l (x:r) in z' : lefts z'
--         rights z@(Zipper _ x (r:rs)) = let z' = Zipper (x:l) r rs in z' : rights z'
--       (경계에서는 빈 리스트)
--     * duplicate (Zipper l x r) = Zipper (lefts ...) (Zipper l x r) (rights ...)
--     * extract = 가운데 값.
--     * 여기서 보조 함수를 밖에 두는 이유: 재귀 구조를 눈에 보이게 하려고.
-- ---------------------------------------------------------------------------

lefts :: Zipper a -> [Zipper a]
lefts = undefined

rights :: Zipper a -> [Zipper a]
rights = undefined

instance Functor Zipper where
  fmap = undefined

instance Comonad Zipper where
  extract = undefined

  duplicate = undefined

-- ---------------------------------------------------------------------------
-- 예제 3. Store — (함수 + 포커스) = "룩업 테이블 위의 커서"
--
--   Store s a 는 's 공간 전체에 대한 값' 과 '현재 위치' 를 가진다.
--   데이터베이스 커서, 이미지의 한 픽셀, 퍼즐의 현재 상태라고 생각하면 된다.
-- ---------------------------------------------------------------------------

data Store s a = Store (s -> a) s

-- ---------------------------------------------------------------------------
-- TODO 3. Store 의 Functor / Comonad 인스턴스
--
--   힌트:
--     * fmap f (Store g x) = Store (f . g) x        -- 함수만 합성
--     * extract (Store g x) = g x                   -- 현재 위치의 값
--     * duplicate (Store g x) = Store (\x' -> Store g x') x
--       ("각 위치에서의 Store 전체" 를 만드는 함수가 새 Store 의 룩업)
--     * 아래 peek / seek / pos / experiment 도 완성해 봐라.
-- ---------------------------------------------------------------------------

instance Functor (Store s) where
  fmap = undefined

instance Comonad (Store s) where
  extract = undefined

  duplicate = undefined

peek :: s -> Store s a -> a
peek s (Store g _) = g s

seek :: s -> Store s a -> Store s a
seek s (Store g _) = Store g s

pos :: Store s a -> s
pos (Store _ s) = s

{- | Store 위에서 "여러 위치를 한 번에 탐색" 하는 함수.
  Applicative f 를 써서, 모든 위치의 값을 효과 안에서 모은다.
  예: experiment (\s -> [s-1, s+1]) store  -- 양옆 값의 리스트
-}
experiment :: (Functor f) => (s -> f s) -> Store s a -> f a
experiment = undefined

-- ---------------------------------------------------------------------------
-- TODO 4. 셀룰러 오토마타 — Comonad 의 전형적 용례
--
--   배경:
--     1차원 셀룰러 오토마타는 "각 셀의 다음 상태 = 이웃 셀들의 함수" 로 정의된다.
--     이 "모든 셀에 같은 국소 규칙을 적용" 하는 작업이 정확히 extend 다.
--
--   목표:
--     a) average3 :: Zipper Double -> Double  (왼쪽, 현재, 오른쪽의 평균)
--     b) stepCA   :: (Zipper a -> b) -> Zipper a -> Zipper b   (= extend, 한 줄)
--     c) rule     :: Zipper Bool -> Bool       -- 직접 규칙을 정해 봐라
--        예: "셋 중 정확히 하나만 참이면 다음에 참"
--     d) runCA    :: Int -> Zipper Bool -> [Zipper Bool]
--        iterate stepCA 를 n 번.
-- ---------------------------------------------------------------------------

average3 :: Zipper Double -> Double
average3 = undefined

stepCA :: (Zipper a -> b) -> Zipper a -> Zipper b
stepCA = undefined

rule :: Zipper Bool -> Bool
rule = undefined

runCA :: Int -> Zipper Bool -> [Zipper Bool]
runCA = undefined

-- ---------------------------------------------------------------------------
-- TODO 5. Store 로 "퍼즐" 풀기 (선택)
--
--   힌트:
--     * Store 위에서 extend 를 반복하면 "룩업 테이블을 소비하는 계산" 이 된다.
--     * 예: 1D 퍼즐에서 각 칸의 값 = 그 칸이 가리키는 칸의 값 + 1.
--       이런 꼬리 재귀적 정의를 extend 로 표현할 수 있다.
--     * seek 로 커서를 옮기고, experiment 로 주변을 본다.
--       이 두 개가 있으면 Store 는 작은 명령형 세계가 된다.
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- TODO 6 (사고 실험)
--
--   a) Comonad 는 "왜" Monad 의 듀얼인가? 타입을 거울에 비춰 보라.
--      join :: m (m a) -> m a      ↔  duplicate :: w a -> w (w a)
--      (>>=) :: (a -> m b) -> m a -> m b   ↔  extend :: (w a -> b) -> w a -> w b
--   b) Monad 는 "효과를 순서대로 쌓는다". Comonad 는 무엇을 쌓는가?
--   c) Data.Stream (패키지 streams) 과 Control.Comonad.Store 를 찾아보라.
--      실제로는 Comonad 가 어디에 쓰이는가? (신경망의 레이어, 퍼즐, 셀룰러,
--      그래프 순회, stencil 계산 등)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex06: Comonad ==="

  -- TODO 1
  let ones = sIterate id 1 :: Stream Integer
      nats = sIterate (+ 1) 0 :: Stream Integer
  print (sTake 5 ones) -- [1,1,1,1,1]
  print (extract ones) -- 1
  print (sTake 3 (duplicate ones)) -- 첫 3 개 부분스트림의 길이 3
  print (sTake 5 (extend (\s -> extract s * 2) nats)) -- [0,2,4,6,8]

  -- TODO 2
  let z = Zipper [3, 2] 1 [4, 5]
  print (extract z) -- 1
  print (toListZ (maybe z id (zipLeft z))) -- [1,2,3,4,5]
  print (length (lefts z), length (rights z)) -- (2,2)

  -- TODO 3
  let store = Store (\n -> n * n) 3
  print (extract store) -- 9
  print (peek 5 store) -- 25
  print (pos (seek 10 store)) -- 10
  print (experiment (\n -> [n - 1, n + 1]) store) -- [4,16]

  -- TODO 4
  let cz = Zipper [0, 0] 1 [0, 0] :: Zipper Double
  print (average3 cz) -- 0.333...
  print (toListZ (stepCA average3 cz)) -- 평균이 퍼진 모양
  let czb = Zipper [True] False [True] :: Zipper Bool
  print (map (toListZ . fmap (fromEnum)) (take 3 (runCA 3 czb))) -- 규칙에 따라
  putStrLn "TODO 1~4 를 채우면 위 출력이 주석과 같아야 한다."
