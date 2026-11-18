{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Ex08Lens
Description : 연습 8 — Profunctor 와 Lens (van Laarhoven optics)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수/인스턴스의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex08-lens

  배경:
    "렌즈" 는 겉보기에 그냥 게터/세터 쌍이다.
      type Lens s a = (s -> a, s -> a -> s)     -- (보기, 고치기)
    하지만 이 표현은 *합성* 이 못 생겼다. van Laarhoven 형식은 이렇게 쓴다:

      type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

    "a 를 f b 로 바꾸는 함수" 를 받아서, s 를 f t 로 바꾸는 함수를 돌려준다.
    f 는 "어떤 효과" 다:
      * f = Const a  -> 출력을 버리고 "읽기" 만 남는다  -> view
      * f = Identity -> "고치기"                        -> over / set
      * f = []       -> "모든 포커스의 리스트"           -> toListOf
    즉 *하나의* 정의로 view/set/over/toListOf 가 전부 파생된다.

    Traversal 은 같은 모양에 Applicative 를 요구한다(포커스가 여러 개).
    Iso/Prism 은 프로펑터(profunctor)를 쓴다(입출력 타입이 자유로운 함수).

    여기서는 프로펑터 클래스를 직접 정의한다(패키지 의존 없이).
    실제로는 Data.Profunctor(profunctors 패키지)와 Control.Lens 를 쓴다.
-}
module Ex08Lens (
  main,
) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))

-- ---------------------------------------------------------------------------
-- 타입 별명 — 광학(optics)의 가족
--
--   Lens      : 포커스가 정확히 하나.   Functor 제약.
--   Traversal : 포커스가 0 개 이상.    Applicative 제약.
--   Setter    : 고치기 전용.           f 를 Identity 로 고정한 특수한 경우.
--   Iso       : "완전히 같은" 두 타입 사이의 변환. Profunctor 제약.--
--   기억해 둘 것: 이 타입들이 전부 "함수를 받아 함수를 돌려주는" 형태라는 점.
--   광학은 결국 "고차 함수의 합성" 이다.
-- ---------------------------------------------------------------------------

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

{- | Setter 는 Traversal/Lens 를 Identity 로 *특수화* 한 것이다.
  그래서 Lens 나 Traversal 은 모두 Setter 자리에 쓸 수 있다(더 일반적이므로).
-}
type Setter s t a b = (a -> Identity b) -> s -> Identity t

type Setter' s a = Setter s s a a

type Iso s t a b = forall p. (Profunctor p) => p a b -> p s t

{- | Iso 의 쓰임: p = (->) 로 인스턴스화하면 "변환기" 가 된다.
  렌즈와의 차이에 주목: 렌즈는 Functor f 로 매개화하고, Iso 는 Profunctor p 로
  매개화한다. 실제 lens 라이브러리는 둘을 한 계층으로 합쳐 "Iso 를 렌즈
  자리에 쓸 수 있게" 만들지만, 그건 훨씬 정교한 타입 트릭이다
  (forall p f. (Profunctor p, Functor f) => p a (f b) -> s -> f t).
-}

-- ---------------------------------------------------------------------------
-- TODO 1. Lens 의 세 가지 연산
--
--   목표:
--     view :: Lens' s a -> s -> a              -- 읽기
--     set  :: Setter' s a -> a -> s -> s       -- 고치기
--     over :: Setter' s a -> (a -> a) -> s -> s -- 함수로 고치기
--
--   힌트 (이게 van Laarhoven 형식의 묘미):
--     * p 를 Const a 로 고르면 출력이 버려지고 "읽기" 만 남는다:
--         view l = getConst . l Const
--     * p 를 Identity 로 고르면 "고치기" 가 된다:
--         over l f = runIdentity . l (Identity . f)
--     * set 은 over 의 특수한 경우다:  set l a = over l (const a)
--     * Signature 에 rank-2 타입이 있으므로 `view = undefined` 대신
--       `view l = undefined` 처럼 인자를 받는 형태로 써야 한다.
--       (GHC 가 다형 타입을 추론하지 못하는 유명한 함정)
-- ---------------------------------------------------------------------------

view :: Lens' s a -> s -> a
view l = undefined

set :: Setter' s a -> a -> s -> s
set = undefined

over :: Setter' s a -> (a -> a) -> s -> s
over = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. 렌즈 만들기
--
--   목표:
--     a) lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
--        힌트:  \f s -> fmap (setter s) (f (getter s))
--        읽고(getter) -> 수정자 적용(f) -> 쓰기(setter). Functor f 가 필요하다.
--     b) _1, _2 :: 튜플의 한 자리를 보는 렌즈.
--        힌트:  _1 f (a, c) = (, c) <$> f a
--               _2 f (a, c) = (a, ) <$> f c
--        ($>)? 아니다 — fmap 으로 구조를 보존한다는 점을 느껴봐라.
--     c) 실전 예제: 중첩된 레코드.
-- ---------------------------------------------------------------------------

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens = undefined

_1 :: Lens (a, c) (b, c) a b
_1 = undefined

_2 :: Lens (a, c) (a, b) c b
_2 = undefined

data Address = Address
  { street :: String
  , city :: String
  }
  deriving (Eq, Show)

data Person = Person
  { pName :: String
  , pAge :: Int
  , pAddr :: Address
  }
  deriving (Eq, Show)

{- | 주소의 도시를 보는 렌즈. 레코드 업데이트 문법과 비교해 봐라:
  person { pAddr = (pAddr person) { city = newCity } }
  vs  set cityLens newCity person
-}
cityLens :: Lens' Person String
cityLens = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. Traversal — "여러 개" 를 동시에 고치기
--
--   배경:
--     Lens 는 포커스가 정확히 하나. Traversal 은 0 개 이상.
--     차이는 Functor -> Applicative 로 강해진다는 것.
--     덕분에 "여러 위치의 효과를 합성" 할 수 있다.
--
--   목표:
--     a) traversed :: Traversable t => Traversal (t a) (t b) a b
--        힌트: 타입이 traverse 와 *똑같이* 생겼다. 그대로 쓸 수 있다.
--     b) both :: Traversal (a, a) (b, b) a b
--        힌트:  \f (x, y) -> (,) <$> f x <*> f y
--     c) toListOf :: Traversal' s a -> s -> [a]
--        힌트: f 를 Const [a] 로 고른다:
--              getConst . t (Const . pure)
--              Const [a] 의 Applicative 인스턴스가 있어야 하는데,
--              그건 Monoid [a] 덕분이다(Applicative (Const m) 는 Monoid m 을 요구).
--     d) (선택) setOf / mapMOf
-- ---------------------------------------------------------------------------

traversed :: (Traversable t) => Traversal (t a) (t b) a b
traversed = undefined

both :: Traversal (a, a) (b, b) a b
both = undefined

toListOf :: Traversal' s a -> s -> [a]
toListOf t = undefined

-- ---------------------------------------------------------------------------
-- TODO 4. Profunctor 와 Iso
--
--   배경:
--     Functor    f  : 출력 쪽만 mapping.   fmap  :: (a -> b) -> f a -> f b
--     Profunctor p  : 양쪽을 mapping.       dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
--     함수 (->) 는 대표적인 프로펑터:  dimap f g h = g . h . f
--
--     Iso 는 "두 타입이 실은 같은 것" 이라는 주장이다.
--       type Iso s t a b = forall p. Profunctor p => (a -> p a b) -> s -> p s t
--       iso :: (s -> a) -> (b -> t) -> Iso s t a b
--     렌즈와의 차이: 렌즈는 "부분을 본다", Iso 는 "같은 것을 다른 각도로 본다".
--     예: newtype 으로 감싼 타입, 튜플의 순서, 문자열과 바이트 배열.
--
--   목표:
--     a) class Profunctor ... 아래 시그니처를 보고 클래스를 완성하라.
--        (lmap/rmap 기본 구현은 그대로 두고 dimap 만 채워도 된다)
--     b) instance Profunctor (->)
--     c) iso :: (s -> a) -> (b -> t) -> Iso s t a b
--        힌트:  dimap sa bt    -- 그게 전부다. 한 줄.
--        "s 를 a 로 보고, b 를 t 로 되돌린다" 는 주장을 프로펑터 언어로 옮긴 것.
--     d) (선택) Choice 프로펑터를 직접 정의하고 _Right 프리즘을 만들어 봐라.
--        Prism 은 Sum 타입의 렌즈다: "매칭 + 생성" 을 한 번에 표현한다.
-- ---------------------------------------------------------------------------

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

  lmap :: (a' -> a) -> p a b -> p a' b
  lmap f = dimap f id

  rmap :: (b -> b') -> p a b -> p a b'
  rmap = dimap id

instance Profunctor (->) where
  dimap = undefined

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa bt

-- ---------------------------------------------------------------------------
-- TODO 5. 실제 패키지와 비교
--
--   이 프로젝트는 `lens` 패키지를 이미 의존성으로 가지고 있다.
--   아래 주석을 풀고, 직접 만든 것과 라이브러리 것을 비교해 봐라.
--
--     import Control.Lens
--     -- view cityLens person == view (person . addr . city) person ?
--     -- 라이브러리는 _1, at, ix, to, unto, ... 수십 개의 조합기를 제공한다.
--     -- 직접 만든 것으로도 원리는 전부 이해했다. 라이브러리는 그걸 공장처럼 만든 것이다.
--     -- (.): 광학의 합성. (<>~): State 렌즈.  이런 것들이 전부 같은 타입의 변주다.
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex08: Profunctor / Lens ==="

  let person =
        Person
          { pName = "김하스켈"
          , pAge = 30
          , pAddr = Address{street = "테헤란로 123", city = "서울"}
          }

  -- TODO 1~2
  print (view cityLens person) -- "서울"
  print (set cityLens "부산" person) -- city 가 "부산" 으로
  print (over cityLens (++ " 특별시") person) -- "서울 특별시"
  print (over _1 (+ 1) (10, 20)) -- (11,20)
  print (view _2 (10, 20)) -- 20

  -- TODO 3
  print (toListOf traversed [1, 2, 3]) -- [1,2,3]
  print (over traversed (* 2) [1, 2, 3]) -- [2,4,6]
  print (over both negate (1, 2)) -- (-1,-2)
  print (toListOf both (1, 2)) -- [1,2]

  -- TODO 4: Iso — p = (->) 로 인스턴스화해서 쓴다
  let strInt = iso read show :: Iso String String Int Int
      swapIso = iso (\(a, b) -> (b, a)) (\(b, a) -> (a, b))
  print (strInt negate "42") -- "-42"  (읽고, 부정하고, 출력)
  print (swapIso (\(a, b) -> (negate a, negate b)) (1, 2 :: Int)) -- (-1,-2)
  putStrLn "TODO 1~4 를 채우면 위 출력이 주석과 같아야 한다."
