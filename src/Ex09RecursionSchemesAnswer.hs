{-# LANGUAGE DeriveFunctor #-}

{- |
Module      : Ex09RecursionSchemes
Description : 연습 9 — 재귀 스킴(recursion schemes): cata / ana / hylo / para

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex09-recursion-schemes

  배경:
    모든 재귀 자료구조는 "한 단계(펼침) + 되먹임(재귀)" 두 부분으로 나뉜다.
    예를 들어 트리의 합계는
      * 트리를 "리프 또는 두 자식" 으로 펼치는 부분 (구조)
      * 자식들의 결과를 더하는 부분 (대수, algebra)
    이 둘을 분리하면, *구조는 재사용* 하고 *계산만 갈아끼울* 수 있다.
    그 분리를 일반화한 것이 재귀 스킴이다.

      Fix f        = 재귀의 고정점 (구조 자체)
      cata alg     = 한 번에 접어 내리기 (fold, 평가)
      ana coalg    = 한 번에 펼쳐 올리기 (unfold, 생성)
      hylo alg c   = 펼쳐서 바로 접기 (생성과 평가를 한 번에 — 중간 트리 없음)
      para alg     = 원래 구조를 버리지 않는 fold (자기 자신 + 자식 결과를 모두 사용)

    실제로는 `recursion-schemes` 패키지가 이 모든 것을 제공한다.
    여기서는 직접 만든다. 만들고 나면 "fold 는 왜 보편적인가" 가 보인다.
-}
module Ex09RecursionSchemes (
  main,
) where

-- ---------------------------------------------------------------------------
-- TODO 1. Fix — 재귀의 고정점
--
--   배경:
--     자료구조 T 를 T = F T 라는 방정식의 해로 본다.
--     그 해가 Fix F 다. 예: ExprF Int 가 한 단계라면, Fix ExprF 가 전체 표현식.
--
--       newtype Fix f = Fix { unFix :: f (Fix f) }
--
--   목표: 아래 newtype 은 이미 정의돼 있다. unFix 의 역할을 이해하고,
--         Fix 의 Functor-ish 한 사용법(생성/분해)을 익혀라.
--
--   힌트:
--     * newtype 인 이유: 런타임 비용이 0 이어야 하므로.
--     * unFix 로 한 겹 벗기고, fmap 으로 자식들에게 재귀를 전파하는 게
--       모든 재귀 스킴의 공통 동작이다.
-- ---------------------------------------------------------------------------

newtype Fix f = Fix {unFix :: f (Fix f)}

-- ---------------------------------------------------------------------------
-- 예제 구조: 산술 표현식의 한 단계
-- ---------------------------------------------------------------------------

data ExprF r
  = LitF Int
  | AddF r r
  | MulF r r
  | NegF r
  deriving (Eq, Show, Functor)

type Expr = Fix ExprF

-- | 리프 값을 만드는 조합기
lit :: Int -> Expr
lit n = Fix (LitF n)

-- ---------------------------------------------------------------------------
-- TODO 2. cata — 카타모피즘(접기)
--
--   목표: alg 를 리프까지 재귀 적용해서 값을 계산한다.
--
--   힌트:
--     * 한 줄이다:
--         cata alg = alg . fmap (cata alg) . unFix
--     * 읽는 순서: 벗기고(unFix) -> 자식들을 재귀(fmap (cata alg)) -> 합치기(alg)
--     * 이게 foldr 의 완전 일반형이다. 리스트의 foldr, 트리의 합계,
--       XML 의 요약, 전부 이 한 줄의 특수한 경우.
-- ---------------------------------------------------------------------------

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- ---------------------------------------------------------------------------
-- TODO 3. ana — 아나모피즘(펼치기)
--
--   목표: 씨앗(seed) 값에서 구조를 생성한다.
--
--   힌트:
--     * cata 의 반대:
--         ana coalg = Fix . fmap (ana coalg) . coalg
--     * coalg :: a -> f a  (한 단계 펼치기)
--     * 이걸로 "1 부터 n 까지 더하는 트리" 같은 걸 만들 수 있다.
--       리스트의 unfoldr, 트리의 생성, L-System, 파서의 생성 쪽이 전부 여기.
-- ---------------------------------------------------------------------------

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- ---------------------------------------------------------------------------
-- TODO 4. hylo — 하일로모피즘(생성과 평가를 한 번에)
--
--   배경:
--     ana 로 만들고 cata 로 접으면 중간 트리가 통째로 생긴다.
--     hylo 는 그 둘을 fuse 해서 중간 자료구조를 만들지 않는다.
--     (이 "fusion" 아이디어가 Haskell 성능 최적화의 핵심 중 하나다:
--      build/foldr 규칙, stream fusion 등이 같은 발상이다.)
--
--   힌트:
--     * hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
--     * 중간에 Fix 가 없다는 점을 확인해 봐라. 그래서 메모리를 안 쓴다.
-- ---------------------------------------------------------------------------

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

-- ---------------------------------------------------------------------------
-- TODO 5. para — 파라모피즘(원래 구조를 아는 fold)
--
--   배경:
--     cata 에서는 자식의 "결과값" 만 받는다. 하지만 때로는 자식의
--     "원래 구조" 도 필요하다. 예: "모든 서브트리의 크기" 를 구할 때.
--
--   힌트:
--     * alg :: f (Fix f, a) -> a
--     * para alg = alg . fmap (\fx -> (fx, para alg fx)) . unFix
--     * 이 "자기 자신 + 결과" 패턴이 바로 파라모피즘이다.
--     * para 로 한 번에 "평가 + 노드 수 세기" 를 동시에 할 수 있다.
--       cata 로는 두 번 트리를 돌아야 한다(또는 상태를 억지로 끌어들여야 한다).
-- ---------------------------------------------------------------------------

para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para alg = alg . fmap (\fx -> (fx, para alg fx)) . unFix

-- ---------------------------------------------------------------------------
-- TODO 6. 계산 모음 — 같은 트리를 다른 대수로 접기
--
--   목표:
--     a) evalAlg    : ExprF Int -> Int            (cata 로 평가)
--     b) sizeAlg    : ExprF Int -> Int            (cata 로 노드 수)
--     c) depthAlg   : ExprF Int -> Int            (cata 로 깊이)
--     d) evalAndSize: ExprF (Int, Int) -> (Int, Int) (cata 로 한 번에 두 값)
--        힌트: 한 번의 cata 에서 튜플을 운반하면 된다. 서로 독립적인 두 대수를
--        합성할 수 있다는 게 포인트(Applicative/Monoid 를 써도 된다).
--     e) evalAndSizePara :: ExprF (Fix ExprF, (Int, Int)) -> (Int, Int)
--        — para 버전. cata 로 한 번에 되는 걸 para 로도 해 보고,
--        언제 para 가 필요한지 느껴봐라.
--     f) buildChain :: Int -> Expr                (ana 로 1+2+...+n 트리 생성)
--        힌트: coalg n = if n <= 0 then LitF 0 else AddF (LitF n) ...?
--              ana coalg 1  형태로. 서브트리 하나는 씨앗에서 재귀 생성한다.
--     g) foldChain :: Int -> Int                  (hylo: 생성 없이 바로 합계)
--        힌트: hylo alg coalg n  (coalg 가 ExprF Int 를 만든다)
-- ---------------------------------------------------------------------------

evalAlg :: ExprF Int -> Int
evalAlg (LitF n) = n
evalAlg (AddF x y) = x + y
evalAlg (MulF x y) = x * y
evalAlg (NegF x) = -x

sizeAlg :: ExprF Int -> Int
sizeAlg (LitF _) = 1
sizeAlg (AddF x y) = 1 + x + y
sizeAlg (MulF x y) = 1 + x + y
sizeAlg (NegF x) = 1 + x

depthAlg :: ExprF Int -> Int
depthAlg (LitF _) = 1
depthAlg (AddF x y) = 1 + max x y
depthAlg (MulF x y) = 1 + max x y
depthAlg (NegF x) = 1 + x

evalAndSize :: ExprF (Int, Int) -> (Int, Int)
evalAndSize (LitF n) = (n, 1)
evalAndSize (AddF (a, sa) (b, sb)) = (a + b, sa + sb + 1)
evalAndSize (MulF (a, sa) (b, sb)) = (a * b, sa + sb + 1)
evalAndSize (NegF (a, sa)) = (-a, sa + 1)

evalAndSizePara :: ExprF (Fix ExprF, (Int, Int)) -> (Int, Int)
evalAndSizePara (LitF n) = (n, 1)
evalAndSizePara (AddF (_, (a, sa)) (_, (b, sb))) = (a + b, sa + sb + 1)
evalAndSizePara (MulF (_, (a, sa)) (_, (b, sb))) = (a * b, sa + sb + 1)
evalAndSizePara (NegF (_, (a, sa))) = (-a, sa + 1)

data Seed = SLeaf Int | SChain Int
buildChain :: Int -> Expr
buildChain n = ana coalg (SChain n)
 where
  coalg :: Seed -> ExprF Seed
  coalg (SLeaf k) = LitF k
  coalg (SChain k)
    | k <= 1 = LitF k
    | otherwise = AddF (SLeaf k) (SChain (k - 1))

foldChain :: Int -> Int
foldChain n = hylo alg coalg (SChain n)
 where
  coalg :: Seed -> ExprF Seed
  coalg (SLeaf k) = LitF k
  coalg (SChain k)
    | k <= 1 = LitF k
    | otherwise = AddF (SLeaf k) (SChain (k - 1))
  alg :: ExprF Int -> Int
  alg (LitF x) = x
  alg (AddF x y) = x + y
  alg (MulF x y) = x * y
  alg (NegF x) = -x

-- ---------------------------------------------------------------------------
-- TODO 7. (선택) 정렬/최적화에 적용
--
--   배경:
--     "상수 폴딩(constant folding)" 최적화는 정확히 재귀 스킴이다:
--       * 서브트리가 모두 리프면 바로 계산한다 -> 대수
--       * 그렇지 않으면 자식을 재귀 -> cata
--     컴파일러의 최적화 패스들을 전부 재귀 스킴으로 표현할 수 있다.
--     GHC 의 Core 최적화도 이런 식으로 생각할 수 있다(simplifier = 재귀 스킴).
--
--   힌트:
--     * para 로 최적화를 만들면, "원래 트리" 와 "결과 트리" 를 모두 볼 수 있어서
--       "이 서브트리는 상수다" 판정이 쉽다.
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- TODO 8. (사고 실험)
--
--   a) cata 와 para 의 차이를 타입만으로 설명해 봐라.
--   b) hylo 가 중간 트리를 만들지 않는 이유를 타입으로 설명해 봐라.
--   c) Fix 를 newtype 이 아니라 data 로 만들면 무슨 일이 생기는가?
--      (그리고 왜 재귀 스킴 라이브러리는 newtype 을 고집하는가?)
--   d) ana 와 apo(아포모피즘, "조기 종료" 가 가능한 unfold) 의 차이는?
--   e) Mendler/CV-comonad 같은 변형들은 왜 생겼는가? (전체 재귀의 이론적 기반)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex09: Recursion Schemes ==="

  -- 1 + (2 * 3) + (-4)
  let e =
        Fix
          ( AddF
              (Fix (LitF 1))
              ( Fix
                  ( AddF
                      (Fix (MulF (Fix (LitF 2)) (Fix (LitF 3))))
                      (Fix (NegF (Fix (LitF 4))))
                  )
              )
          )

  print (cata evalAlg e) -- 3
  print (cata sizeAlg e) -- 8
  print (cata depthAlg e) -- 4
  print (cata evalAndSize e) -- (3,8)
  print (para evalAndSizePara e) -- (3,8)
  print (cata evalAlg (buildChain 4)) -- 10 (1+2+3+4)
  print (foldChain 4) -- 10
  putStrLn "TODO 2~6 을 채우면 위 출력이 주석과 같아야 한다."
