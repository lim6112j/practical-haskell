{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellVersion (main, addItem, printReceipt, runProgram) where

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
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
type RState a = ReaderT Env (WriterT String (State CartState)) a

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
runProgram :: RState a -> Env -> CartState -> ((a, String), CartState)
runProgram prog env st = runState (runWriterT (runReaderT prog env)) st

program :: RState String
program = addItem "coffee" 4 >> addItem "milk" 2 >> printReceipt

main :: IO ()
main = do
  let env = Env{taxRate = 0.1, currency = "€"}
      emptyCart = CartState{items = [], totalTax = 0}
      ((receipt, log), finalState) = runProgram program env emptyCart
  putStrLn log
  putStrLn receipt
  print finalState

-- => ("coffee€, milk€ | 세금 합계: 0.60€", CartState {items = ["coffee","milk"], totalTax = 0.6})
