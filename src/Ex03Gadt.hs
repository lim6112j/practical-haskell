{-|
Module      : Ex03Gadt
Description : 연습 3 — GADTs, 타입 안전 표현식, 존재 양화(existential)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run ex03-gadt

  이 모듈이 컴파일되려면 파일 상단의 확장 플래그가 필요하다.
  (cabal 에 -XGADTs 등을 추가해 두었는지 package.yaml 을 확인하라.)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Ex03Gadt
  ( main
  ) where

-- ---------------------------------------------------------------------------
-- 타입 안전 표현식 AST
--
--   일반 data 선언과의 차이: 생성자의 "결과 타입" 을 직접 쓴다.
--    data Expr a where
--      I   :: Int  -> Expr Int
--      Add :: Expr Int -> Expr Int -> Expr Int
--    이제 `Add (I 1) (B True)` 는 "문법 오류" 가 아니라 "타입 오류" 다.
--    잘못된 프로그램을 실행 시 오류가 아니라 컴파일 시점에 죽인다.
--
--   힌트: `If` 생성자는 양쪽 가지가 같은 타입 a 여야 하면서
--         조건은 Bool 이다. 이 타입을 GADT 문법으로 어떻게 쓰는지가 핵심.
-- ---------------------------------------------------------------------------

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  Leq :: Expr Int -> Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

-- | Show 인스턴스는 직접 손으로 쓴다.
--   (GADT 에 대한 deriving 은 문맥 추론이 까다로워, 여기서는 수동으로 제공한다.)
--   패턴 매칭만으로 충분하다는 점에 주목: 내부 타입 a 를 몰라도 된다.
instance Show a => Show (Expr a) where
  showsPrec d e = case e of
    I n -> showParen (d > 10) $ showString "I " . showsPrec 11 n
    B b -> showParen (d > 10) $ showString "B " . showsPrec 11 b
    Add x y -> showParen (d > 10) $ showString "Add " . showsPrec 11 x . showString " " . showsPrec 11 y
    Mul x y -> showParen (d > 10) $ showString "Mul " . showsPrec 11 x . showString " " . showsPrec 11 y
    Not x -> showParen (d > 10) $ showString "Not " . showsPrec 11 x
    Leq x y -> showParen (d > 10) $ showString "Leq " . showsPrec 11 x . showString " " . showsPrec 11 y
    If c x y -> showParen (d > 10) $ showString "If " . showsPrec 11 c . showString " " . showsPrec 11 x . showString " " . showsPrec 11 y

-- ---------------------------------------------------------------------------
-- TODO 1. eval
--
--   목표: Expr a 를 "완전히" 계산해서 a 를 돌려준다.
--         부분 함수가 아니다 — 모든 경우를 커버해야 한다.
--
--   힌트:
--     * GADT 패턴 매칭에서는 타입 정보가 함께 흐른다.
--       예: If 의 두 가지는 둘 다 Expr a 이고 결과도 a 다.
--     * 타입을 믿고 직접 재귀하면 된다. `error` 나 `undefined` 가
--       하나라도 필요하면 타입 설계가 잘못된 것이다.
--     * 여기서 완전성(exhaustiveness)이 보장된다는 게 일반 인터프리터와의 차이다.
-- ---------------------------------------------------------------------------

eval :: Expr a -> a
eval = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. 타입 동등성 증명(type equality witness)
--
--   배경:
--     컴파일러는 "Expr Bool 은 Expr Int 가 될 수 없다" 고 안다.
--     하지만 런타임에 어떤 타입인지 모르는 값이 들어오면, 타입을
--     "증명" 해야만 안전하게 다룰 수 있다. 그 증명물이 TypeEq a b 이다.
--
--   목표:
--     a) cast    : 증명을 받아 타입을 바꾼다.
--     b) sameInt : 증명 없이 Expr a 가 Int 표현식인지 판정한다.
--        (타입이 맞으면 Just Refl, 아니면 Nothing)
--     c) evalInt : sameInt 를 이용해 임의의 Expr a 를 Maybe Int 으로 평가.
--
--   힌트:
--     * Refl 하나만 있으면 a 와 b 가 같은 타입임을 컴파일러가 안다.
--       패턴 매칭 한 번으로 `x :: a` 를 `b` 로 쓸 수 있다.
--     * sameInt 는 B 생성자에서만 Nothing 이다 (나머지는 전부 Expr Int).
--     * 이것이 Data.Type.Equality (base) 의 원형이다. 한번 찾아보라.
-- ---------------------------------------------------------------------------

data TypeEq a b where
  Refl :: TypeEq a a

cast :: TypeEq a b -> a -> b
cast = undefined

sameInt :: Expr a -> Maybe (TypeEq a Int)
sameInt = undefined

evalInt :: Expr a -> Maybe Int
evalInt = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. 존재 양화(existential) — "무언가" 를 상자에 담기
--
--   배경:
--     SomeExpr 은 "내부가 어떤 타입인지는 숨겨져 있지만, Expr 임" 이라는 타입.
--     박스를 열기 전까지는 내부 타입을 알 수 없다(= 사용할 수 없다).
--     이것이 타입 클래스 제약과 만나면 "어떤 타입의 값이든, Show 만 되면" 이라는
--     표현이 가능해진다.
--
--   목표:
--     a) showSome : 박스를 열어서 문자열로 만든다.
--        힌트: 패턴 매칭으로 a 타입을 꺼내면 Show a 제약이 생긴다.
--              eval 도 타입이 generic 하므로 그대로 쓸 수 있다.
--     b) mkSome   : 문자열을 아주 단순하게 파싱한다.
--        "true"/"false" -> B, 숫자 -> I, 그 외 -> Nothing.
--        (readMaybe 를 쓰면 숫자 파싱은 한 줄이다)
-- ---------------------------------------------------------------------------

data SomeExpr where
  SomeExpr :: Show a => Expr a -> SomeExpr

showSome :: SomeExpr -> String
showSome = undefined

mkSome :: String -> Maybe SomeExpr
mkSome = undefined

-- ---------------------------------------------------------------------------
-- TODO 4 (선택, 사고 실험). 아래 주석을 풀고 컴파일해 봐라.
--
--   -- 이 줄은 컴파일되지 않는다:
--   -- bad = Add (I 1) (B True)
--
--   질문:
--     a) 일반 data 로 만든 (느슨한) AST 에서는 이 프로그램이 어떻게 됐는가?
--     b) 인터프리터에서 이 경우를 처리하려면 어떤 꼼수가 필요한가?
--        (dynamic 타입? 태그? partiality?)
--     c) GADT 버전에서 인터프리터가 왜 더 짧고 안전해지는가?
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex03: GADTs / 타입 안전 표현식 ==="

  let e1 = Add (I 1) (Mul (I 2) (I 3))            -- 1 + 2*3
      e2 = If (Leq (I 3) (I 4)) (I 100) (I 0)     -- 3 <= 4 ? 100 : 0
      e3 = Not (Leq (I 3) (I 4))

  print (eval e1)                -- 7
  print (eval e2)                -- 100
  print (eval e3)                -- False

  print (evalInt e1)             -- Just 7
  print (evalInt e3)             -- Nothing (Bool 표현식이다)

  print (showSome (SomeExpr e1)) -- "7"  (eval 결과를 show)
  print (fmap showSome (mkSome "42")) -- Just "42"
  print (fmap showSome (mkSome "hello")) -- Nothing

  putStrLn "TODO 1~3 을 채우면 위 출력이 주석과 같아야 한다."
