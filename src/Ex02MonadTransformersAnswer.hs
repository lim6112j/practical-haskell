{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Ex02MonadTransformers
Description : 연습 2 — Monad Transformers (StateT / ExceptT / ReaderT) 와 mtl 스타일

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run ex02-monad-transformers
-}
module Ex02MonadTransformersAnswer (
  main,
) where

import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), lift)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Except (ExceptT, runExcept)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, modify, runStateT)

-- ---------------------------------------------------------------------------
-- 상황 설정: 설정값 + 누적 상태 + 실패 가능성 + IO
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgName :: String
  , cfgStep :: Int
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config{cfgName = "ex02", cfgStep = 10}

-- ---------------------------------------------------------------------------
-- TODO 1. 가장 작은 스택: StateT + Except
--
--   목표: "상태를 누적하다가, 조건에 걸리면 에러로 중단" 하는 계산을 만들어라.
--
--   타입 설명:
--     type Calc = StateT Int (Except String)
--     StateT s m a  ≅  s -> m (a, s)     -- 상태를 받아, (결과, 새 상태) 를 돌려줌
--     ExceptT e m a ≅  m (Either e a)    -- 에러를 입힌 모나드
--     즉 Calc a ≅ Int -> Either String (a, Int)
--
--   힌트:
--     * addTo :: Int -> Calc () 는 상태에 더한다.
--       StateT 안에서는 get / put / modify 를 그대로 쓸 수 있다 (한 겹이라 lift 불필요).
--     * checkNotNegative :: Calc () 는 상태가 음수면 throwError 로 실패.
--       Except 는 StateT 의 "안쪽" 이므로 여기서는 `lift . throwError` 가 필요하다.
--       (겹의 순서가 바뀌면 필요한 lift 개수도 바뀐다 — 이게 핵심 감각.)
--     * runCalc :: Calc a -> Int -> Either String (a, Int)
--       = runState ...  -- 순서를 생각해 봐라. runStateT 가 StateT 를 벗기고
--         ExceptT 를 남기므로, 그 결과를 runExceptT 로 벗겨야 한다.
--         힌트: runStateT m s0 :: Except String (a, Int)  →  runExceptT 로 마무리.
-- ---------------------------------------------------------------------------

type Calc = StateT Int (Except String)

addTo :: Int -> Calc ()
addTo n = do
  modify (+ n)

checkNotNegative :: Calc ()
checkNotNegative = do
  s <- get
  if s < 0
    then throwError "Should not negative"
    else put s

runCalc :: Calc a -> Int -> Either String (a, Int)
runCalc c n = do
  runExcept (runStateT c n)

-- ---------------------------------------------------------------------------
-- TODO 2. 세 겹 스택 + IO
--
--   목표: ReaderT(설정) / StateT(카운터) / ExceptT(에러) / IO 를 쌓고,
--         제대로 벗기는 run 함수를 만든다.
--
--   힌트:
--     * 타입은 이미 정해져 있다. 어떤 연산이 몇 번의 lift 를 필요로 하는지
--       직접 타입을 보고 세어 보라:
--         ask       -> 0 번 (ReaderT 가 제일 바깥)
--         get/put   -> 1 번 (StateT 는 ReaderT 안)
--         throwError-> 2 번 (ExceptT 는 StateT 안)
--         putStrLn  -> 3 번 (IO 가 제일 안)
--       맞춰 보면서 왜 그런지 이해하는 게 이 연습의 전부다.
--     * `runApp` 는 벗기는 순서가 중요하다. 안쪽부터 차례로:
--         runExceptT -> runStateT -> runReaderT  (조합 순서는 자유, 타입이 안내한다)
--     * 프로그램 예시: 현재 카운터를 읽고, cfgStep 을 더하고, 그 값을 출력.
-- ---------------------------------------------------------------------------

type App = ReaderT Config (StateT Int (ExceptT String IO))

bumpAndLog :: App ()
bumpAndLog = do
  c <- ask
  s <- lift get
  lift $ put (s + cfgStep c)
  lift $ lift $ lift $ putStrLn ("log : " ++ show (s + cfgStep c))

runApp :: Config -> Int -> App a -> IO (Either String (a, Int))
runApp cfg n app = do
  runExceptT (runStateT (runReaderT app cfg) n)

-- ---------------------------------------------------------------------------
-- TODO 3. mtl 스타일로 다시 쓰기
--
--   배경:
--     위에서는 "구체적인 스택" 에 대해 코딩했다. mtl 은 이걸 일반화한다.
--       class Monad m => MonadReader r m | m -> r where ask :: m r
--       class Monad m => MonadState s m | m -> s where get :: m s
--       class Monad m => MonadError e m | m -> e where throwError :: e -> m a
--     GHC 가 각 스택에 대해 인스턴스를 자동으로 만들어 주기 때문에,
--     lift 없이 ask / get / throwError 를 섞어 쓸 수 있다.
--
--   목표: TODO 2 의 bumpAndLog 를 "어떤 모나드에서든" 동작하게 일반화하라.
--
--   힌트:
--     * 시그니처에 제약을 걸면 된다:
--         bumpGeneric :: (MonadReader Config m, MonadState Int m, MonadIO m) => m ()
--       (MonadError 는 쓰지 않는다 — 실패하지 않으므로. 쓸데없는 제약을
--        다는 습관은 피하라.)
--     * 이 함수는 App 에서도, 단순히 ReaderT Config (StateT Int IO) 에서도,
--       나중에 아예 다른 스택에서도 그대로 돌아간다. 그게 mtl 의 가치.
--     * concrete 한 버전과 generic 한 버전의 타입을 옆에 놓고 비교해 봐라.
-- ---------------------------------------------------------------------------

bumpGeneric ::
  (MonadReader Config m, MonadState Int m, MonadIO m) =>
  m ()
bumpGeneric = do
  c <- ask
  s <- get
  put (cfgStep c + s)
  liftIO $ putStrLn ("Generic log : " ++ show (cfgStep c + s))

-- ---------------------------------------------------------------------------
-- TODO 4 (선택). transformer 순서의 의미
--
--   질문에 답해 보라 (주석으로 답을 적든, 직접 타입을 바꿔서 실험하든):
--     a) StateT Int (Except String) 와 ExceptT String (StateT Int) 는
--        계산이 실패했을 때 어떻게 다른가? (상태가 보존되는가?)
--     b) 왜 하필 ExceptT 를 StateT 안에 넣는 쪽을 더 자주 쓰는가?
--     c) IO 를 제일 안쪽에 두는 이유는?
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex02: Monad Transformers ==="

  -- TODO 1
  print (runCalc (addTo 5 >> addTo 3) 0) -- Right ((),8)
  print (runCalc (addTo 5 >> addTo 100 >> checkNotNegative) 0)
  print (runCalc (addTo 5 >> addTo (-100) >> checkNotNegative) 0)
  -- Left "..."  (에러 메시지는 직접 정해서 채워라)

  -- TODO 2
  r <- runApp defaultConfig 0 bumpAndLog
  print r -- Right ((),10)

  -- TODO 3
  (_, n) <- runStateT (runReaderT bumpGeneric defaultConfig) 0
  print n -- 10
  putStrLn "TODO 1~3 을 모두 채우면 위 출력이 주석과 같아야 한다."
