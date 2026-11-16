{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Ex05ST
Description : 연습 5 — ST 모나드와 국소적 가변성 (그리고 IO 와의 관계)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run ex05-st
-}
module Ex05STAnswer (
  main,
) where

import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)

-- ---------------------------------------------------------------------------
-- 배경: 순수 함수형 언어에 "변수" 가 있다?
--
--   Haskell 에는 두 종류의 가변성이 있다.
--     * IO 안의 가변성  : IORef, 프로그램 전체에 걸쳐 살고, 실행 순서가 의미를 가진다.
--     * ST 안의 가변성 : STRef, 계산이 끝나면 사라지고, 밖에서는 순수 함수로 보인다.
--
--   ST 의 마법: `runST :: (forall s. ST s a) -> a`
--     s 는 "상태 스레드" 라는 타입 레벨 토큰이다. STRef s a 안의 s 가
--     바깥으로 새어나오지 못하게 막아서, 가변성이 "국소적" 이게 된다.
--     결과적으로 runST 로 얻은 값은 완전히 순수한 값이다.
--
--   핵심 감각:
--     "가변성은 구현 전략이고, 순수성은 인터페이스다."
--     ST 는 이 둘을 동시에 만족시키는 유일한 틈새.
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- TODO 1. 제일 단순한 예: 참조를 하나 만들고, 더하고, 읽기
--
--   목표: ref 를 0 으로 만들고, 1 부터 n 까지 더한 뒤 값을 돌려준다.
--         결과는 순수한 Int 여야 한다.
--
--   힌트:
--     * newSTRef :: a -> ST s (STRef s a)
--     * modifySTRef' 는 없다: modifySTRef 를 쓰면 된다
--       (modifySTRef ref (+ x) 형태). 단, 이건 느슨하게 쌓인다 —
--        Ex04 를 떠올려라. n 이 크면 writeSTRef 로 직접 쓰는 편이 낫다.)
--     * 타입 서명은 이미 정해져 있다. `s` 가 밖으로 노출되지 않는다는 점을
--       확인하라: 서명에 s 가 없다.
-- ---------------------------------------------------------------------------

sumViaRef :: Int -> Int
sumViaRef n = runST $ do
  ref <- newSTRef 0
  let go !i
        | i > n = readSTRef ref
        | otherwise = do
            x <- readSTRef ref
            writeSTRef ref $! x + i -- $! 로 강제: modifySTRef 처럼 thunk 를 쌓지 않는다
            go (i + 1)
  go 1

-- ---------------------------------------------------------------------------
-- TODO 2. 배열 없는 정렬 — 선택 정렬을 STRef 로
--
--   배경:
--     순수한 정렬은 새 리스트를 만든다(불변). ST 안에서는 리스트를
--     "참조의 리스트" 로 보고, 제자리에서 교환하며 정렬할 수 있다.
--     겉으로는 같은 입력->출력 함수지만, 내부는 명령형이다.
--
--   목표: 선택 정렬을 ST 안에서 구현하라.
--
--   힌트:
--     * 전략: 입력 리스트를 STRef 의 리스트로 변환한다.
--         refs <- mapM newSTRef xs    -- mapM 은 ST 모나드에서도 쓸 수 있다
--     * 최소값의 인덱스를 찾고, 그 위치와 교환한다.
--       인덱스 접근은 리스트라서 O(n) — 그래도 연습용으로 충분하다.
--     * 마지막에 mapM readSTRef refs 로 값을 다시 모은다.
--     * 순수 버전과 결과가 같아야 한다: sortPure [3,1,2] == [1,2,3]
--     * (심화) Data.Array.ST / STArray 를 찾아보라. STRef 리스트보다
--       훨씬 자연스럽고 빠르다.
-- ---------------------------------------------------------------------------

sortPure :: (Ord a) => [a] -> [a]
sortPure = undefined -- 정렬 알고리즘은 직접. 선택 정렬을 권장.

sortViaST :: (Ord a) => [a] -> [a]
sortViaST xs = runST (undefined)

-- ---------------------------------------------------------------------------
-- TODO 3. IO vs ST — 같은 코드, 다른 세계
--
--   배경:
--     IO 와 ST 는 둘 다 "명령형" 이지만, ST 는 탈출구(runST)가 있다.
--     runST 안에서는 IO 액션을 쓸 수 없다. 반대로 IO 안에서는
--     ST 계산을 runST 로 언제든 실행할 수 있다.
--     이 경계가 "순수한 함수" 와 "부수효과가 있는 함수" 를 갈라낸다.
--
--   목표: 아래를 완성하고, 타입을 옆에 적어보라.
--
--   힌트:
--     * pureCount :: Int -> Int 는 ST 안에서 센다.
--     * ioCount   :: Int -> IO Int 는 IORef 로 센다.
--       (Data.IORef: newIORef, modifyIORef', readIORef)
--     * 둘의 차이는 타입만이 아니라 "무엇을 보장하는가" 이다.
--       pureCount 는 어떤 문맥에서 부르든 같은 값을 준다(참조 투명).
--       ioCount 는 호출할 때마다 새 계산이 수행된다.
-- ---------------------------------------------------------------------------

pureCount :: Int -> Int
pureCount = undefined

ioCount :: Int -> IO Int
ioCount = undefined

-- ---------------------------------------------------------------------------
-- TODO 4 (사고 실험). 아래 질문에 답해 봐라.
--
--   a) 왜 runST 의 타입에 `forall s.` 가 필요한가? 없으면 어떤 일이 생기는가?
--      (힌트: 런타임에 "살아 있는" 참조를 돌려주는 함수를 만들어 봐라.
--       왜 그게 타입 오류가 되는지 이해하면 ST 를 이해한 것이다.)
--   b) ST 안에서 예외가 나면 참조들은 어떻게 되는가?
--   c) 왜 STRef 대신 STArray 를 실무에서 더 자주 쓰는가?
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex05: ST / 국소적 가변성 ==="

  let xs = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: [Int]
  print (sumViaRef 100) -- 5050
  print (sortPure [5, 2, 9, 1, 7]) -- [1,2,5,7,9]
  print (sortViaST [5, 2, 9, 1, 7]) -- [1,2,5,7,9]
  print (sortPure xs == sortViaST xs) -- True
  print (pureCount 1000) -- 1000
  n <- ioCount 1000
  print n -- 1000
  putStrLn "TODO 1~3 을 채우면 위 출력이 주석과 같아야 한다."
