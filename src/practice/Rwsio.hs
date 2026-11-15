{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Rwsio
  ( main,
  addItem,
  printReceipt,
  runProgram,
  runCheckout,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (
  RWS,
  RWST,
  ask,
  get,
  mapRWST,
  put,
  runRWS,
  runRWST,
  tell,
 )
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Text.Printf (printf)

--
-- interface Env { taxRate: number; currency: string; }
-- interface CartState { items: string[]; totalTax: number; }
--
data Env = Env
  { taxRate :: Float
  , currency :: String
  }
  deriving (Show)

data CartState = CartState
  { items :: [String]
  , totalTax :: Float
  }
  deriving (Show)

-- type Reader<Env, A> = (env: Env) => A;
-- type State<S, A>    = (s: S) => [A, S];
--
-- // 결합: Env를 읽고 CartState를 갱신하며 값을 돌려줌
-- type RState<A> = Reader<Env, State<CartState, A>>;
--

-- // ---------- 기본 조합자 ----------
-- const ask: RState<Env> = env => s => [env, s];
-- const get: RState<CartState> = env => s => [s, s];
-- const put = (s: CartState): RState<undefined> =>
--   _env => _old => [undefined, s];
-- const modify = (f: (s: CartState) => CartState): RState<undefined> =>
--   env => s => [undefined, f(s)];
--
-- RWST는 Reader + Writer + State를 한 번에 합친 모나드다:
type RState a = RWS Env String CartState a

addItem :: String -> Float -> RState String
addItem item price = do
  env <- ask
  let tax = taxRate env * price
  oldState <- get
  let newState =
        CartState{items = items oldState ++ [item], totalTax = totalTax oldState + tax}
  put newState
  let itemLine = item ++ currency env
  tell itemLine
  pure itemLine

printReceipt :: RState String
printReceipt = do
  env <- ask
  oldState <- get
  pure
    ( printf
        "%s | 세금 합계: %.2f%s"
        (intercalate ", " (items oldState))
        (totalTax oldState)
        (currency env)
    )

-- // ---------- 실제 로직 ----------
-- const addItem = (item: string, price: number): RState<string> =>
--   env => s => {
--     const tax = price * env.taxRate;
--     return [`${item}${env.currency}`,
--             { items: [...s.items, item], totalTax: s.totalTax + tax }];
--   };
--
-- const printReceipt: RState<string> =
--   env => s =>
--     `${s.items.join(", ")} | 세금 합계: ${s.totalTax.toFixed(2)}${env.currency}`;
--
-- // ---------- 실행 ----------
-- const emptyCart: CartState = { items: [], totalTax: 0 };
-- const myEnv: Env = { taxRate: 0.1, currency: "€" };
--
-- const step1 = addItem("coffee", 4)(myEnv)(emptyCart);
-- const step2 = addItem("milk", 2)(myEnv)(step1[1]);
-- console.log(printReceipt(myEnv)(step2[1])[0]);
-- // => "coffee€, milk€ | 세금 합계: 0.60€"
runProgram :: RState a -> Env -> CartState -> (a, String, CartState)
-- mtl 2.3+의 runRWS는 (값, 최종 상태, 로그) 순서로 돌려주므로 로그를 중간으로 옮긴다
runProgram prog env st =
  let (result, finalState, wlog) = runRWS prog env st
   in (result, wlog, finalState)

program :: RState String
program = addItem "coffee" 4 >> addItem "milk" 2 >> printReceipt

main :: IO ()
main = do
  let env = Env{taxRate = 0.1, currency = "€"}
      emptyCart = CartState{items = [], totalTax = 0}
      (receipt, wlog, finalState) = runProgram program env emptyCart
  putStrLn wlog
  putStrLn receipt
  print finalState
  print "app io"
  (a, b, c) <- runCheckout env emptyCart
  print a
  print b
  print c

--------------------------------------------------------------------------------
-- RWST ... IO : 진짜 IO 효과가 섞인 버전
--------------------------------------------------------------------------------

{- | RWS와 구조는 똑같지만 베이스 모나드가 Identity에서 IO로 바뀌었다.

RWS r w s a      == r -> s -> (a, w, s)       -- 순수 함수
RWST r w s IO a  == r -> s -> IO (a, w, s)    -- IO 효과 추가

RWS는 사실 RWST r w s Identity의 특수한 경우다 (mtl도 실제로 그렇게 정의).
-}
type AppIO a = RWST Env String CartState IO a

{- | 사용자에게 아이템과 가격을 직접 물어보는 함수.

- liftIO로 콘솔 입출력 (IO 효과)
- ask/get/put/tell은 lift 없이 그대로 동작 (RWS 층의 효과)
-}
askAndAddItem :: AppIO ()
askAndAddItem = do
  liftIO (putStr "아이템 이름: ")
  item <- liftIO getLine
  liftIO (putStr "가격: ")
  price <- liftIO (readLn :: IO Float)
  env <- ask
  oldState <- get
  let tax = taxRate env * price
      newState =
        CartState{items = items oldState ++ [item], totalTax = totalTax oldState + tax}
  put newState
  tell (item ++ currency env)

{- | 순수 RWS 프로그램(printReceipt)을 IO 스택 안에서 재사용한다.

lift :: Monad m => m a -> t m a
는 "베이스 모나드를 변환기로" 올리는 함수라서, 이미 변환기인
RState (= RWST ... Identity)를 RWST ... IO로 바꾸는 데는 쓸 수 없다.
그럴 때는 mapRWST로 안쪽 모나드를 직접 바꾼다:

  mapRWST :: (Identity (a, s, w) -> n (a, s, w))
          -> RWST r w s Identity a -> RWST r w s n a
-}
printReceiptIO :: AppIO String
printReceiptIO = mapRWST (pure . runIdentity) printReceipt

-- 커피/우유를 대화형으로 담고 영수증을 출력하는 IO 프로그램
checkout :: AppIO ()
checkout = do
  askAndAddItem
  askAndAddItem
  receipt <- printReceiptIO
  liftIO (putStrLn receipt)

{- | 실행: runRWST는 IO를 돌려주므로 main에서 직접 쓸 수 있다.

이 mtl 버전의 runRWST도 runRWS처럼 IO (값, 상태, 로그) 순서라서 재배치한다.
-}
runCheckout :: Env -> CartState -> IO ((), String, CartState)
runCheckout env st = do
  (result, finalState, wlog) <- runRWST checkout env st
  pure (result, wlog, finalState)

-- main에서 쓰는 방법 (대화형이라 기본 main에서는 주석으로 남겨둠):
--
--   main = do
--     let env = Env{taxRate = 0.1, currency = "€"}
--     (_, wlog, finalState) <-
--       runCheckout env CartState{items = [], totalTax = 0}
--     print wlog        -- Writer 층 결과 (데이터로도 받을 수 있음)
--     print finalState  -- State 층 결과

-- => ("coffee€, milk€ | 세금 합계: 0.60€", CartState {items = ["coffee","milk"], totalTax = 0.6})
