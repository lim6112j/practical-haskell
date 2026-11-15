{-|
Module      : Ex04Laziness
Description : 연습 4 — 게으름(laziness), thunk, 공간 누수, 엄격성(strictness)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run ex04-laziness
      stack run ex04-laziness -- +RTS -s      # 메모리/GC 통계까지 보기
-}

{-# LANGUAGE BangPatterns #-}

module Ex04Laziness
  ( main
  ) where

import Data.List (foldl')

-- ---------------------------------------------------------------------------
-- TODO 1. 느린 합계 vs 빠른 합계
--
--   배경:
--     Haskell 의 모든 값은 "계산되지 않은 계산(thunk)" 이다.
--     `foldl (+) 0 [1..n]` 은 매 단계 (0+1), ((0+1)+2), ... 라는
--     thunk 를 n 개 쌓고, 마지막에 한 번에 평가한다.
--     즉 공간도 시간도 낭비. 이게 "foldl 은 거의 쓸모없다" 는 말의 이유.
--     foldl' 은 각 단계에서 강제로 평가해 thunk 를 쌓지 않는다.
--
--   목표: BangPatterns 를 써서 느린 foldl 과 같은 결과를 내는
--         엄격한 합계 함수를 직접 만들어라.
--
--   힌트:
--     * 보조 함수의 누산기 인자에 `!` 를 붙이면, 함수에 들어올 때
--       WHNF(weak head normal form) 로 강제 평가된다.
--         go !acc (x:xs) = go (acc + x) xs
--     * `acc + x` 자체가 즉시 평가되는 건 아니다. BangPatterns 는
--       "다음 호출 시점" 에 강제하는 것뿐이다. 그래도 충분하다:
--       이전 thunk 를 다음 단계에서 깨버리므로 쌓이지 않는다.
--     * seq 을 쓰는 버전도 같이 써 보고 차이를 느껴봐라.
-- ---------------------------------------------------------------------------

sumLazy :: Int -> Int
sumLazy n = foldl (+) 0 [1 .. n]

sumStrict :: Int -> Int
sumStrict = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. 튜플 누적의 공간 누수
--
--   아래 함수는 결과는 맞지만 공간을 낭비한다.
--   왜 그런지 주석으로 설명하고, 엄격한 버전을 만들어라.
--
--   힌트:
--     * foldl 의 누산기가 튜플이면, 각 단계마다
--         ((0,0) + ...) 형태의 thunk 튜플이 n 개 생긴다.
--     * 해법 1: 엄격한 필드를 가진 데이터 타입을 만든다.
--         data Acc = Acc !Int !Int   -- 느낌표가 핵심
--       `!` 는 생성자가 "엄격한 필드" 를 가지게 한다(StrictData 없이도).
--     * 해법 2: seq 로 튜플 전체를 강제한다.
--         let p = ... in p `seq` p
--       (튜플은 seq 으로 WHNF 까지만 강제되고, 내부까지는 안 간다 —
--        이 한계가 해법 1 이 더 나은 이유다.)
--     * foldl' 를 쓰는 것만으로는 부족하다 (누산자가 lazy 하므로).
-- ---------------------------------------------------------------------------

statsLazy :: [Int] -> (Int, Int)
statsLazy = foldl (\(s, n) x -> (s + x, n + 1)) (0, 0)

data Acc = Acc !Int !Int
  deriving (Eq, Show)

statsStrict :: [Int] -> (Int, Int)
statsStrict = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. seq 과 Weak Head Normal Form
--
--   배경:
--     seq a b  : a 를 WHNF 로 평가한 뒤 b 를 돌려준다.
--     WHNF 란 "최소한의 평가" 다:
--       * 정수/문자 -> 완전히 평가된 값
--       * 생성자 c x1 x2 -> 생성자까지 (내부는 thunk 일 수 있음)
--       * (\x -> ...) -> 람다 (몸체는 평가 안 됨)
--     즉 seq (1 + 1) (2 + 2) 는 2 를 먼저 평가하고 4 를 돌려준다.
--     seq 은 타입 클래스가 아닌 "프리미티브" 라는 점도 중요하다.
--
--   목표: 아래 두 함수를 완성하고, 그 결과와 이유를 주석으로 적어라.
--
--   힌트:
--     * `a `seq` b` 에서 a 가 필요 없어 보여도 평가가 강제된다.
--     * (x, x) 에서 x 를 한 번만 평가하고 싶다면 let ... in 과 seq 을 조합.
--     * deepseq (패키지) 은 WHNF 가 아니라 완전 평가를 한다.
--       NFData 가 뭔지 찾아보라.
-- ---------------------------------------------------------------------------

forceAdd :: Int -> Int -> Int
forceAdd a b = undefined -- a 와 b 를 모두 강제 평가한 뒤 더하라

-- | 튜플의 첫 요소만 강제 평가하고 싶을 때:
firstForced :: Int -> Int -> (Int, Int)
firstForced a b = undefined -- 첫 요소는 WHNF 로, 둘째는 thunk 로 남겨라

-- ---------------------------------------------------------------------------
-- TODO 4. 게으름의 장점 — 무한 자료구조
--
--   배경:
--     게으름은 버그가 아니라 기능이다. 무한한 값을 "묘사" 하고,
--     필요한 만큼만 소비할 수 있다.
--
--   목표: 아래 함수들을 완성하라.
--
--   힌트:
--     * 자연수 무한 스트림은 [1..] 로 이미 존재한다.
--     * `cycle [1,2,3]` 은 무한 반복.
--     * fibs 는 자기 참조로 정의한다:  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
--      (이 "knotted" 정의가 lazy 하기 때문에 성립한다)
--     * 조건을 만족하는 첫 요소:  `find` 나 `head . filter`.
--       filter 가 게으르게 동작하므로 무한 리스트에서도 멈춘다.
-- ---------------------------------------------------------------------------

fibs :: [Integer]
fibs = undefined

firstOver :: Integer -> Integer
firstOver n = undefined -- n 보다 큰 첫 피보나치 수

-- ---------------------------------------------------------------------------
-- 참고: 게으름으로 인한 함정들
--
--   * `length` 가 O(1) 이 아니다 (리스트를 끝까지 평가해야 함)
--   * `Data.Map` 은 값이 thunk 인 채로 쌓일 수 있다
--   * 예외는 평가 시점까지 미뤄져서, "고장 난 코드" 가 멀리서 터진다
--   * foldr 은 무한 리스트를 소비할 수 있고 foldl 은 못 한다 (순서 차이)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex04: Laziness / strictness ==="

  let n = 200000
  print (sumLazy n == sumStrict n)   -- True
  print (sumLazy n == foldl' (+) 0 [1 .. n]) -- True (느린 것도 결과는 같다)
  print (statsLazy [1 .. 10])        -- (55,10)
  print (statsStrict [1 .. 10])      -- (55,10)
  print (forceAdd 3 4)               -- 7
  -- firstForced 의 "둘째가 thunk 인지" 는 print 로는 보이지 않는다.
  -- 평가 시점은 디버거(:force, :sprint)나 +RTS -s 할당량으로 확인하라.
  print (firstForced 1 2)            -- (1,2)

  print (take 10 fibs)               -- [0,1,1,2,3,5,8,13,21,34]
  print (firstOver 100)              -- 144
  putStrLn "TODO 1~4 를 채우면 위 출력이 주석과 같아야 한다."
