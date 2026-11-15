module ArrowPractice
  ( main
  ) where

import Control.Arrow
import Data.Char (toUpper)

inc :: Integer -> Integer
inc = \x -> x + 1
dub :: Integer -> Integer
dub = \x -> x * 2

{- | (>>>)  : f >>> g = g . f          -- 연결
     (***) : f *** g는 (x, y) -> (f x, g y)  -- 튜플 두 자리에 각각 적용
     (&&&) : f &&& g는 x -> (f x, g x)       -- 같은 입력을 두 함수에
-}

-- | 1. arr: 순수 함수를 "어떤 Arrow에든" 끌어올린다.
--    덕분에 파이프라인을 Arrow 타입에 대해 일반적으로(다형적으로) 작성할 수 있다.
scale :: (Arrow a, Num n) => n -> a n n
scale k = arr (* k) >>> arr (+ 1)

-- (->) 화살표로 인스턴스화하면 그냥 순수 함수
pureUse :: Integer -> Integer
pureUse = scale 2

-- 같은 scale 코드를 Kleisli IO 화살표로도 인스턴스화 — IO 효과 안에서 재사용
ioUse :: Integer -> IO Integer
ioUse x = runKleisli (scale 2 :: Kleisli IO Integer Integer) x

-- | 2. Kleisli: IO 동작도 화살표다. (>>>)로 효과를 파이프라인으로 연결하고,
--    중간중간 arr로 순수 함수를 그대로 끼워 넣을 수 있다.
echoUpper :: Kleisli IO String String
echoUpper =
  Kleisli (\s -> putStrLn ("입력 받음: " ++ s) >> pure s)
    >>> arr (map toUpper)
    >>> Kleisli (\s -> putStrLn ("대문자 변환: " ++ s) >> pure s)

-- | 3. ArrowChoice (left / right / +++): Either 값이 파이프라인을 분기해서 흐른다.
--    left는 Left 경로만 가공하고 Right는 그대로 통과시킨다.
route :: Either String Integer -> Either String Integer
route = left (++ "!")

-- | 4. first / second: 튜플의 한쪽 자리에만 화살표를 적용하고 나머지는 통과.
onlyDoubleFirst :: (Integer, Integer) -> (Integer, Integer)
onlyDoubleFirst = first dub

-- | 5. &&&의 응용: 한 입력으로 여러 파생값을 만들어 튜플로 묶기
tagWithLength :: String -> (String, Int)
tagWithLength = id &&& length

main :: IO ()
main = do
  -- *** : 두 함수를 튜플의 각 자리에 각각 적용
  print ((inc *** dub) (3, 4))              -- (4, 8)

  -- &&& : 하나의 입력을 두 함수에 뿌려서 튜플로
  print ((inc &&& dub) 5)                   -- (6, 10)

  -- >>> : 합성. inc 하고 나서 dub => dub (inc 3) = 8
  print ((inc >>> dub) 3)                   -- 8

  -- 조합: 같은 입력으로 inc와 dub을 하고 결과를 더함
  print ((inc &&& dub >>> uncurry (+)) 5)   -- 6 + 10 = 16

  putStrLn "--- Arrow의 특장점 ---"

  -- arr: 동일한 scale 코드가 순수 세계와 IO 세계 양쪽에서 동작
  print (pureUse 10)                        -- 21
  ioUse 10 >>= print                        -- 21 (같은 코드, Kleisli IO)

  -- Kleisli: IO 효과를 >>>로 연결한 파이프라인
  runKleisli echoUpper "haskell"            -- 콘솔 로그 + "HASKELL"

  -- ArrowChoice: Either 분기 처리를 합성으로
  print (route (Left "hi"))                 -- Left "hi!"
  print (route (Right 5))                   -- Right 5

  -- first: 튜플 첫 자리만 변형
  print (onlyDoubleFirst (3, 4))            -- (6, 4)

  -- &&&: 파생값 묶기
  print (tagWithLength "haskell")           -- ("haskell",7)
