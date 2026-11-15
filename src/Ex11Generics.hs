{-|
Module      : Ex11Generics
Description : 연습 11 — GHC.Generics 와 DerivingVia: 보일러플레이트를 지우는 두 도구

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수/인스턴스의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex11-generics

  배경:
    Haskell 에는 "모든 데이터 타입은 몇 개의 조합으로 만들어진다" 는 사실이 있다.
      data Person = Person { name :: String, age :: Int }
      -- 구조적으로: D1 (C1 (S1 (K1 String) :*: S1 (K1 Int)))
      data Shape = Circle Double | Rect Double Double
      -- 구조적으로: D1 (C1 (S1 (K1 Double)) :+: C1 (S1 (K1 Double) :*: S1 (K1 Double)))

    GHC.Generics 는 이 "구조 표현(Rep)" 을 타입 클래스로 노출한다.
    그러면 *한 번* 구조에 대한 처스를 작성하면 모든 타입에 재사용할 수 있다.
    JSON 인코더, Eq, Show, 비교, 직렬화... 전부 이 원리다.

    DerivingVia 는 다른 도구다:
      "이미 있는 인스턴스를 새 타입에 빌려 쓴다."
      newtype 감싸기 + 인스턴스 = 인스턴스 조립. 타입 레벨의 조합(combinator)이다.

  구성:
    클래스의 default 시그니처, M1 D/S 통과 인스턴스, DerivingVia 절은
    '주어진 코드' 다. 직접 고쳐 보며 실험해 봐라.
    진짜 연습은 TODO 로 표시된 인스턴스 몸통들이다.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Ex11Generics
  ( main
  ) where

import Data.Char (toUpper)
import Data.Monoid (Sum (..))
import Data.Semigroup (Dual (..))
import GHC.Generics

-- ---------------------------------------------------------------------------
-- 인코딩 대상: 아주 작은 JSON 비슷한 값
-- ---------------------------------------------------------------------------

data Value
  = VNull
  | VBool Bool
  | VInt Integer
  | VStr String
  | VList [Value]
  | VObj [(String, Value)]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- TODO 1. 원시 타입의 ToValue 인스턴스
--
--   목표: 아래 인스턴스들을 완성하라.
--     * Integer, Int -> VInt
--     * Double       -> VInt . round  (간단히)
--     * String       -> VStr
--     * Bool         -> VBool
--     * [a]          -> VList (ToValue a 필요)
--     * Maybe a      -> VNull / toValue a
--
--   힌트:
--     * 이 인스턴스들이 "바닥" 이다. Generics 는 결국 여기로 수렴한다.
--     * [a] / Maybe a 인스턴스에는 FlexibleContexts 가 필요하다
--       (제약이 단순 타입 변수가 아니므로). 파일 상단에 이미 켜져 있다.
-- ---------------------------------------------------------------------------

class ToValue a where
  toValue :: a -> Value

  -- | 기본 구현: Generic 구조를 한 줄로 펼친다. (주어진 코드)
  --   이것이 DefaultSignatures 의 묘미다: 인스턴스를 *비워서* 선언해도
  --   GHC 가 이 기본 구현을 사용한다.
  default toValue :: (Generic a, GFields (Rep a)) => a -> Value
  toValue = VList . toRow

instance ToValue Integer where
  toValue = undefined

instance ToValue Int where
  toValue = undefined

instance ToValue Double where
  toValue = undefined

-- | 주의: String 은 [Char] 의 별칭이라, 아래 인스턴스와
--   `instance ToValue a => ToValue [a]` 가 겹친다.
--   이럴 때 쓰는 것이 {-# OVERLAPPING #-} 프래그머다.
--   (이 "String = [Char] 때문에 생기는 인스턴스 충돌" 은 Haskell 의
--    고전적인 함정이다. OverlappingInstances 확장으로 전체를 느슨하게
--    만들 수도 있지만, 그건 권장하지 않는다 — 가능한 한 좁게 표시하라.)
instance {-# OVERLAPPING #-} ToValue String where
  toValue = undefined

instance ToValue Bool where
  toValue = undefined

instance ToValue a => ToValue [a] where
  toValue = undefined

instance ToValue a => ToValue (Maybe a) where
  toValue = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. Generics 마찰: "구조를 한 줄로 펼치는" 클래스
--
--   배경:
--     Rep a 는 a 의 구조 표현이다. 모든 Rep 은 다음 조합기로 만들어진다:
--       U1      : 정보 없음 (값이 없는 자리)
--       K1 i c  : 실제 값 하나 (필드)
--       M1 i c f: 메타데이터 감싸기 — i 가 D(데이터타입) / C(생성자) / S(필드명)
--       (:*:)   : 여러 필드의 곱 (레코드, 다중 인자 생성자)
--       (:+:)   : 여러 생성자의 합 (enum, sum type)
--
--   목표: gfields :: f a -> [Value] 를 모든 구조에 대해 작성하라.
--
--   힌트:
--     * U1:  gfields _ = []
--     * K1:  gfields (K1 x) = [toValue x]      -- ToValue c 제약 필요
--     * M1 D / M1 S 는 "메타데이터" 일 뿐이므로 안쪽으로 통과 (주어진 코드).
--       왜 D 와 S 를 *따로* 인스턴스로 만드는지 생각해 봐라:
--       D, C, S 는 서로 다른 kind 의 팬텀 타입이라 인스턴스 헤드가 겹치지 않는다.
--       (M1 i c f 처럼 i 를 변수로 두면 C 용 인스턴스와 겹쳐서 컴파일 오류다)
--     * M1 C:  생성자 이름을 conName 으로 얻어 첫 원소로 넣어라.
--         gfields m@(M1 x) = VStr (conName m) : gfields x
--       이걸로 Sum 타입의 어떤 생성자인지 구분할 수 있다.
--     * (:*:):  gfields (l :*: r) = gfields l ++ gfields r
--     * (:+:):  gfields (L1 x) = gfields x
--                gfields (R1 x) = gfields x
-- ---------------------------------------------------------------------------

class GFields f where
  gfields :: f a -> [Value]

instance GFields U1 where
  gfields = undefined

instance ToValue c => GFields (K1 i c) where
  gfields = undefined

instance GFields f => GFields (M1 D d f) where
  gfields (M1 x) = gfields x

instance GFields f => GFields (M1 S s f) where
  gfields (M1 x) = gfields x

instance (Constructor c, GFields f) => GFields (M1 C c f) where
  gfields = undefined

instance (GFields l, GFields r) => GFields (l :*: r) where
  gfields = undefined

instance (GFields l, GFields r) => GFields (l :+: r) where
  gfields = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. Generic 과 값 세계를 연결하기
--
--   목표: toRow 를 완성하라.
--
--   힌트:
--     * from :: a -> Rep a x   (Generic 클래스의 메서드)
--     * toRow = gfields . from
--     * 이 한 줄로 "모든 Generic 타입" 이 [Value] 으로 변환된다.
--     * 제약 GFields (Rep a) 가 "이 타입의 구조를 우리가 이해한다" 는 뜻이다.
--       Person 이건 Shape 건, K1 이 몇 개든 상관없이 같은 코드가 동작한다.
-- ---------------------------------------------------------------------------

toRow :: (Generic a, GFields (Rep a)) => a -> [Value]
toRow = undefined

-- ---------------------------------------------------------------------------
-- TODO 4. Generic 인스턴스를 쓰는 타입들 (인스턴스 선언은 주어진 코드)
--
--   배경:
--     `instance ToValue Person` 처럼 *빈 몸통* 으로 선언하면,
--     클래스의 default 시그니처가 쓰인다. 이게 DefaultSignatures 다.
--
--   목표:
--     a) Person / Shape 인스턴스가 이미 선언돼 있다. TODO 1~3 을 채우면
--        자동으로 동작하는지 확인해 봐라.
--     b) 필드 이름까지 얻고 싶으면? Selector 클래스의 selName 을 써서
--        M1 S s f 인스턴스를 따로 만들어 봐라:
--          instance (Selector s, GFields f) => GFields (M1 S s f) where
--            gfields m@(M1 x) = VObj [(selName m, ...)] : ...
--        레코드와 위치 인자의 차이를 어떻게 구분할 수 있는지도 생각해 봐라.
--        (힌트: 레코드 필드는 selName 이 의미 있는 문자열을 준다.
--         위치 인자는 빈 문자열이다 — 그걸로 분기하면 된다)
-- ---------------------------------------------------------------------------

data Person = Person
  { pName :: String
  , pAge :: Int
  , pCity :: String
  }
  deriving (Generic, Show, Eq)

data Shape
  = Circle Double
  | Rect Double Double
  deriving (Generic, Show, Eq)

instance ToValue Person

instance ToValue Shape

-- ---------------------------------------------------------------------------
-- TODO 5. DerivingVia — 인스턴스 조립
--
--   배경:
--     DerivingVia 는 "이미 있는 인스턴스를 새 타입에 빌려 쓴다".
--     핵심은 newtype 다: 런타임 비용 0 으로 인스턴스를 갈아끼우는 어댑터.
--
--   (a) Summed / Reversed 는 via 절이 이미 붙어 있다 (주어진 코드).
--       Sum 과 Dual 라이브러리 인스턴스를 빌려 쓰는 예다.
--       Semigroup/Monoid 를 직접 구현하지 않고도 합산/반전 결합이 된다.
--
--   (b) Shout / LengthTag 는 *직접* 해 봐라:
--       * Shout a      : Show 를 대문자로 출력하는 어댑터.
--           newtype Shout a = Shout a deriving Show via ShoutAdapter a
--           instance Show a => Show (ShoutAdapter a) where
--             show (ShoutAdapter a) = map toUpper (show a)
--       * LengthTag [a]: ToValue 를 "길이만" 내보내는 어댑터.
--           instance ToValue (LengthTag [a]) where ...
--       * 완성하면 main 의 주석 처리된 줄을 푼다.
--
--   힌트:
--     * DerivingVia 는 "타입 레벨 조합" 이다. newtype 을 쌓으면 인스턴스를
--       레고처럼 조립할 수 있다.
--     * 단점: 어댑터 인스턴스를 직접 만들어야 한다(보일러플레이트는 남는다).
--     * DerivingStrategies(stock / newtype / anyclass / via)를 구분해 봐라.
-- ---------------------------------------------------------------------------

newtype Summed = Summed Int
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Sum Int

newtype Reversed a = Reversed [a]
  deriving (Eq, Show)
  deriving (Semigroup) via Dual [a]

newtype Shout a = Shout a
  deriving (Eq, Show)

newtype LengthTag a = LengthTag a
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- TODO 6. (사고 실험)
--
--   a) Generics 로 만든 toValue 와 DerivingVia 로 만든 인스턴스의 차이는?
--      (Generics = 구조에서 자동 생성, DerivingVia = 기존 인스턴스 재사용)
--   b) 왜 Generic 인스턴스를 *직접* 쓰지 않고 GFields 같은 중간 클래스를 두는가?
--   c) ToJSON 을 derive 하는 aeson 은 내부적으로 무엇을 하는가?
--      (힌트: GToJSON + Generics. 위와 같은 구조다)
--   d) DerivingVia 로 Monad / Applicative 인스턴스를 조립할 수도 있다.
--      예: 새로운 모나드를 기존 모나드의 조합으로 derive. (DerivingVia 의 진짜 힘)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex11: Generics + DerivingVia ==="

  -- TODO 1
  print (toValue (42 :: Int))            -- VInt 42
  print (toValue "hello")                -- VStr "hello"
  print (toValue True)                   -- VBool True
  print (toValue ([1, 2, 3] :: [Int]))  -- VList [VInt 1,VInt 2,VInt 3]

  -- TODO 2~4
  let p = Person "김하스켈" 30 "서울"
      c = Circle 1.5
      r = Rect 2 3
  print (toRow p)                        -- [VStr "Person",VStr "김하스켈",VInt 30,VStr "서울"]
  print (toRow c)                        -- [VStr "Circle",VInt 1]  (Double 는 반올림)
  print (toRow r)                        -- [VStr "Rect",VInt 2,VInt 3]
  print (toValue p)                      -- 위와 같은 값의 VList 형태
  print (toValue c)

  -- TODO 5
  print (Summed 1 <> Summed 2)                   -- Summed 3
  print (mconcat [Summed 1, Summed 2, Summed 3]) -- Summed 6
  print (Reversed [1, 2] <> Reversed [3])        -- Reversed [3,1,2] (Dual: 순서 반전)

  -- TODO 5-b: 어댑터 인스턴스를 만들고 deriving 절을 붙인 뒤 주석을 푼다.
  -- print (show (Shout "hello"))                 -- "HELLO"
  -- print (toValue (LengthTag [1,2,3] :: [Int])) -- VInt 3

  putStrLn "TODO 1~5 를 채우면 위 출력이 주석과 같아야 한다."
