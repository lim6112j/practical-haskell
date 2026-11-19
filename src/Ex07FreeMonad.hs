{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Ex07FreeMonad
Description : 연습 7 — Free Monad: 효과를 데이터로 만들기

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수/인스턴스의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex07-free-monad

  배경:
    "어떤 Functor f 가 주어지면, 거기서 무료로(공짜로) 모나드를 만들 수 있다."
    그게 Free 모나드다.

      data Free f a = Pure a | Free (f (Free f a))

    핵심 아이디어:
      * Free (TeletypeF ...) 는 "텔레타입 프로그램" 이라는 *구문(syntax)* 이다.
      * 인터프리터(대수, algebra)를 적용하면 *의미(semantics)* 가 된다.
      * 즉 프로그램을 값으로 다룰 수 있다: 저장, 변환, 검사, 테스트, 로깅.

    실제 패키지는 `free` / `freer-simple` / `polysemy` 다. 여기서는 100 줄로
    직접 만들어 본다. 만들고 나면 효과 시스템 라이브러리들이 다 같은 모양이라는
    걸 알게 된다.

    단점도 알아 둬라: 모든 >>= 가 트리 노드를 만드므로, 고성능 코드에는
    일반적으로 알맞지 않다(효과 시스템은 "유연성" 을 사는 것이다).
-}
module Ex07FreeMonad (
  main,
) where

import Control.Monad.State (State, gets, modify', runState)

-- ---------------------------------------------------------------------------
-- TODO 1. Free 모나드의 정의와 인스턴스
--
--   목표: 아래 타입의 Functor / Applicative / Monad 인스턴스를 완성하라.
--
--   힌트:
--     * Functor:  값만 바꾸고, 구조(트리)는 그대로.
--         fmap f (Pure a)   = Pure (f a)
--         fmap f (Free fm)  = Free (fmap (fmap f) fm)
--       (f 안쪽의 Free f a 에 재귀적으로 fmap 을 적용해야 한다)
--     * Applicative: Monad 인스턴스가 있으면 `pure` 와 `ap`/`liftM` 으로
--       유도할 수 있다. 하지만 직접 만들어 보는 편이 감각에 좋다.
--         Pure f <*> x = fmap f x
--         Free fm <*> x = Free (fmap (<*> x) fm)
--     * Monad:  순수 값은 그냥 통과, Free 노드는 자식을 재귀.
--         Pure a >>= k = k a
--         Free fm >>= k = Free (fmap (>>= k) fm)
--     * 이 정의가 "왼쪽 결합" 하는지, 그리고 왜 순수값에서 멈추는지 생각해 봐라.
-- ---------------------------------------------------------------------------

data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap = undefined

instance (Functor f) => Applicative (Free f) where
  pure = undefined

  (<*>) = undefined

instance (Functor f) => Monad (Free f) where
  (>>=) = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. liftF — 한 단계의 효과를 Free 로 끌어올리기
--
--   힌트: Free (fmap Pure fx)  한 줄이다.
--         (f 안쪽의 값들을 전부 Pure 로 감싸서 "더 할 일 없음" 을 표시)
-- ---------------------------------------------------------------------------

liftF :: (Functor f) => f a -> Free f a
liftF = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. foldFree — Free 프로그램을 실제 모나드로 "실행"
--
--   목표: 대수(algebra) 하나로 Free 트리를 접어서 진짜 모나드로 만든다.
--
--   힌트:
--     * alg :: f (m a) -> m a 가 "한 단계를 어떻게 해석할지" 를 알려준다.
--     * Pure a 는 그냥 순수값, Free fm 은 자식을 먼저 재귀해서 m a 로 만든 뒤
--       alg 에 넘긴다.
--     * 이름이 fold 인 이유: cata(카타모피즘) 이기 때문이다. (Ex09 참고)
--     * 이것이 Free 모나드의 전부다. 인터프리터 = foldFree + 대수.
-- ---------------------------------------------------------------------------

foldFree ::
  (Functor f, Monad m) =>
  (f (m a) -> m a) ->
  Free f a ->
  m a
foldFree = undefined

-- ---------------------------------------------------------------------------
-- TODO 4. retract — "진짜 모나드" 로 되돌리기
--
--   배경:
--     Free f 는 f 를 포함하는 가장 작은 모나드다. 그래서 f 자체가 모나드라면
--     Free f 를 f 로 되돌릴 수 있다(retract). 이게 Free 모나드가
--     "무료(free)" 인 이유: 무료한 대상은 망각 함자(재밌는 것을 잊버리는 함수)의
--     왼쪽 수반이다. 범주론적으로는 "왼쪽 수반은 자유로운 구조" 다.
--
--   힌트:
--     * Pure a -> pure a
--     * Free fm -> fm >>= retract     -- f 가 모나드여야 >>= 가 있다
-- ---------------------------------------------------------------------------

retract :: (Monad f, Functor f) => Free f a -> f a
retract = undefined

-- ---------------------------------------------------------------------------
-- 예제 효과: Teletype (입력/출력)
-- ---------------------------------------------------------------------------

data TeletypeF a
  = GetF (String -> a)
  | PutF String a

{- | Functor 인스턴스는 자동 유도가 안 된다.
  왜? `GetF (String -> a)` 에서 a 가 함수의 *인자* 위치(반공변)에 있기 때문.
  반공변 위치는 fmap 으로 다룰 수 없다. 그래서 손으로 쓴다.
  (이게 DeriveFunctor 의 한계이고, Invariant 을 따로 배우는 이유이기도 하다.)
-}
instance Functor TeletypeF where
  fmap f (GetF k) = GetF (f . k)
  fmap f (PutF s a) = PutF s (f a)

-- | 문법을 만들기 위한 스마트 생성자. 이제 do-notation 으로 프로그램을 쓸 수 있다.
getLine' :: Free TeletypeF String
getLine' = liftF (GetF id)

putStrLn' :: String -> Free TeletypeF ()
putStrLn' s = liftF (PutF s ())

-- | 순수하게 묘사된 프로그램. 이 값 자체로는 아무 일도 일어나지 않는다.
greet :: Free TeletypeF ()
greet = do
  putStrLn' "이름이 뭐죠?"
  name <- getLine'
  putStrLn' ("안녕하세요, " ++ name ++ "!")

-- ---------------------------------------------------------------------------
-- TODO 5. 두 개의 인터프리터 (같은 프로그램, 다른 의미)
--
--   목표:
--     a) interpIO     : TeletypeF (IO a) -> IO a         (실제 입출력)
--     b) interpPure   : TeletypeF (State [String] a) -> State [String] a
--        (입력은 상태에서 꺼내 쓰고, 출력은 상태에 append — 순수하고 테스트하기 쉽다)
--     c) runPure      : [String] -> Free TeletypeF a -> ([String], a)
--
--   힌트:
--     * TeletypeF 는 "다음 계산을 담은 함수" 또는 "출력 + 다음 계산" 이라서,
--       인터프리터는 그냥 이어주면 된다.
--         interpIO (GetF k)   = getLine >>= k
--         interpIO (PutF s a) = putStrLn s >> pure a
--     * 순수 버전에서는 getLine 대신 상태에서 입력을 꺼내고, putStrLn 대신
--       상태에 출력을 추가한다. gets / modify' 를 써라.
--     * foldFree interpPure 로 실행한다.
--     * 이것이 "효과를 데이터로 분리" 했을 때의 이득: 한 프로그램, 다중 해석.
--       테스트는 IO 없이, 로깅은 조용히, 실제 실행은 진짜 콘솔로.
-- ---------------------------------------------------------------------------

interpIO :: TeletypeF (IO a) -> IO a
interpIO = undefined

interpPure :: TeletypeF (State [String] a) -> State [String] a
interpPure = undefined

runPure :: [String] -> Free TeletypeF a -> ([String], a)
runPure = undefined

-- ---------------------------------------------------------------------------
-- TODO 6 (선택). 효과를 조합하기
--
--   목표: 두 효과를 합친 Functor 를 만들고, Free 로 프로그램을 써 봐라.
--
--   힌트:
--     * 합성은 `data Sum f g a = InL (f a) | InR (g a)` (Coproduct).
--       이걸 Functor 인스턴스로 만들면 Free (Sum f g) 가 두 효과의 프로그램.
--     * 인터프리터는 각 케이스를 다른 대수로 보내면 된다.
--     * 여기까지 오면 "효과 시스템" 의 핵심을 이해한 것이다:
--       효과 = 문법(Functor) + 대수(해석) + 조합(Coproduct).
--       polysemy / freer-simple 이 바로 이것의 정교한 버전이다.
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex07: Free Monad ==="

  -- TODO 1~4 는 타입 수준에서만 확인한다 (주석을 풀면 타입이 맞아야 한다)
  -- let p = putStrLn' "hi" >> getLine' :: Free TeletypeF String
  -- print (length (show (Free undefined)))

  -- TODO 5
  putStrLn "-- interpIO 로 실행 --"
  foldFree interpIO greet

  putStrLn "-- interpPure 로 실행 --"
  print (runPure ["김하스켈"] greet) -- (["안녕하세요, 김하스켈!"], ())
  putStrLn "TODO 1~5 를 채우면 위 두 실행이 의도대로 동작해야 한다."
