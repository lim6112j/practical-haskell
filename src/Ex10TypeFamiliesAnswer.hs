{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Ex10TypeFamilies
Description : 연습 10 — 타입 수준 프로그래밍: DataKinds, 타입 가족, TypeLits

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  TODO 가 표시된 함수/인스턴스의 `undefined` 를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex10-type-families

  배경:
    여기서부터 "값" 과 "타입" 의 경계가 흐려진다. Haskell 에서 타입은 *계산* 된다.

      * DataKinds     : 데이터 생성자를 타입/kind 로 승격시킨다.
                        'True, 'False, '[], ':'  -- 홑따옴표가 승격 표시
      * 타입 가족     : 타입에 대한 함수.  type family F a where ...
      * TypeLits      : 타입 레벨 숫자(Nat)와 문자열(Symbol)
      * GADTs         : 타입 레벨 정보를 값에 새긴다

    왜 하는가? "잘못된 프로그램을 실행 오류가 아니라 컴파일 오류로" 만들기 위해.
    길이가 타입에 적힌 벡터, 상태가 타입에 적힌 프로토콜, 단위가 적힌 수량 등.

    이 연습이 어려운 이유: 디버거가 없다. 타입 오류 메시지를 읽는 법이 전부다.
    오류 메시지를 "값 계산의 trace" 라고 생각하면 읽기 쉬워진다.

  구성:
    타입 가족(If / Append / Length / Factorial / Max)은 *주어진 코드* 다.
    직접 고쳐 보면서 실험해 봐라(패턴을 추가하거나, 열린 가족으로 바꿔 보거나).
    진짜 연습은 TODO 로 표시된 값 레벨 함수들(HList, Vec)이다.
-}
module Ex10TypeFamiliesAnswer (
  main,
) where

import Data.Proxy (Proxy (..))
import Unsafe.Coerce (unsafeCoerce)

-- \| 타입 레벨 산술 연산자(+ - * <=?)는 모듈 전체를 import 해서 가져온다.
--   값 레벨 연산자와는 다른 이름 공간(namespace)이므로 충돌하지 않는다.
import GHC.TypeLits

-- ---------------------------------------------------------------------------
-- Part A. DataKinds + 닫힌 타입 가족 (주어진 코드 — 직접 고쳐 보며 실험)
--
--   배경:
--     DataKinds 는 `data Bool = True | False` 를 kind Bool 과
--     타입 'True / 'False 로 만든다. 그러면 "타입으로 하는 if" 가 가능해진다.
--
--   읽는 법:
--     * 'True / 'False 는 승격된 생성자.
--     * 닫힌 타입 가족은 모든 경우를 커버해야 한다(완전성이 검사된다).
--     * If 의 k 는 "타입들의 kind" 다. Bool 도, Nat 도, * 도 될 수 있다.
--       즉 이건 타입 레벨의 다형 함수다.
-- ---------------------------------------------------------------------------

type family If (c :: Bool) (t :: k) (e :: k) :: k where
  If 'True t e = t
  If 'False t e = e

type family Not (b :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'False b = 'False
  And a 'False = 'False

-- ---------------------------------------------------------------------------
-- Part B. 타입 레벨 리스트 + 값 레벨 HList
--
--   배경:
--     승격된 리스트: '[Int, Bool]  (kind = [*])
--     타입 가족 Append 로 이어붙이고, 값 레벨 HList 로 그 증거를 만든다.
--     타입과 값이 "함께" 움직이도록 강제하는 게 핵심이다.
-- ---------------------------------------------------------------------------

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

{- | 타입 레벨 리스트를 *타입으로* 가지는 이질적 리스트.
  HNil 은 '[] 에, (:>) 은 (x ': xs) 에 대응한다. 구조와 타입이 1:1.
-}
data HList (xs :: [*]) where
  HNil :: HList '[]
  (:>) :: x -> HList xs -> HList (x ': xs)

infixr 5 :>

-- ---------------------------------------------------------------------------
-- TODO 1. hAppend
--
--   목표: 두 HList 를 이어붙인다. 결과 타입이 Append xs ys 여야 한다.
--
--   힌트:
--     * HNil ys = ys 가 기저.
--     * (x :> xs) ys = x :> hAppend xs ys
--     * 여기서 "타입 가족 Append" 와 "값 재귀" 가 같은 모양으로 따라간다는 게
--       포인트. 타입을 믿고 재귀하면 GHC 가 나머지를 맞춘다.
--     * 왜 이게 중요한가: 타입이 결과의 *모양* 을 서술하므로,
--       잘못된 이어붙음(예: 문자열과 불을 헷갈림)은 컴파일 오류가 된다.
-- ---------------------------------------------------------------------------

hAppend :: HList xs -> HList ys -> HList (Append xs ys)
hAppend HNil ys = ys
hAppend (x:>xs) ys = x :> hAppend xs ys

-- ---------------------------------------------------------------------------
-- TODO 2. 길이 재기
--
--   목표:
--     a) hLengthV :: HList xs -> Int      (값 재귀)
--     b) hLengthT 는 타입 레벨 길이를 런타임 값으로 꺼낸다 (주어진 코드).
--        KnownNat (Length xs) 제약이 "길이가 유한한 수" 라는 보증이다.
--        natVal (Proxy @(Length xs)) — Proxy 로 타입을 "값처럼" 전달한다.
--
--   힌트:
--     * Length 타입 가족이 재귀적으로 정의돼 있어야 hLengthT 의 제약이 풀린다.
--     * 둘의 결과가 항상 같아야 한다. 어느 쪽이 더 "강한" 보장을 주는가?
-- ---------------------------------------------------------------------------

type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

-- | 값 레벨에서 길이를 센다. 타입은 결과에 관여하지 않는다.
--
--   왜 hLengthT 를 그대로 쓸 수 없는가:
--     hLengthT 는 KnownNat (Length xs) 제약을 요구한다. 즉 "이 타입 레벨 리스트의
--     길이를 컴파일 타임에 안다" 는 보증이 필요한데, xs 가 아직 변수이면
--     GHC 는 Length xs 를 Nat 리터럴로 정규화할 수 없어 그 인스턴스를 만들지 못한다.
--     (반대로 main 에서 구체적인 타입을 붙여 부르면 hLengthT 도 잘 돈다.)
--     그래서 값 재귀로 센다. 구조를 따라가므로 모든 xs 에 대해 성립하고,
--     결과는 항상 타입 레벨 Length 와 같다. 타입이 강제하는 쪽은 hLengthT 다.
hLengthV :: HList xs -> Int
hLengthV HNil = 0
hLengthV (_ :> xs) = 1 + hLengthV xs

hLengthT :: forall xs. (KnownNat (Length xs)) => HList xs -> Int
hLengthT _ = fromInteger (natVal (Proxy @(Length xs)))

-- ---------------------------------------------------------------------------
-- Part C. TypeLits — 타입 레벨 숫자 계산
--
--   배경:
--     Nat kind 의 값은 숫자 리터럴 그 자체다:  3 :: Nat
--     GHC 는 타입 레벨 산술(+ - * ^, <=?) 을 *해결기* 로 푼다.
--     KnownNat 클래스는 "이 타입 레벨 숫자의 값을 런타임에 안다" 고 말한다.
-- ---------------------------------------------------------------------------

type family Factorial (n :: Nat) :: Nat where
  Factorial 0 = 1
  -- `*` 는 정규화 불가: `*` 는 "kind Type" 로도 쓰이므로 타입에서
  -- 모호해진다. 그래서 타입 레벨 곱셈은 정규화된 형태로 쓴다.
  -- (이런 함정이 타입 수준 프로그래밍의 일상이다)
  Factorial n = n GHC.TypeLits.* Factorial (n - 1)

factorial5 :: Integer
factorial5 = natVal (Proxy @(Factorial 5))

type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max 0 b = b
  Max a 0 = a
  Max a b = If (a <=? b) b a

-- ---------------------------------------------------------------------------
-- TODO 3. reifyNat — 숫자를 타입 세계로 되돌려 보내기
--
--   목표: 런타임 정수가 유효한 Nat 인지 확인하고, 그 값을 돌려준다.
--
--   힌트:
--     * someNatVal :: Integer -> Maybe SomeNat
--     * SomeNat p 에서 p :: Proxy n 이고, natVal p 로 값을 꺼낸다.
--     * 이게 "reification" 이다: 값을 타입으로 격상시키는 작업.
--       (반대 방향은 hLengthT / vlengthT 같은 "타입을 값으로" 다.)
--     * 실제로는 이걸로 "런타임 값에 따라 타입을 고르는" 패턴을 만든다.
-- ---------------------------------------------------------------------------

reifyNat :: Integer -> Maybe Integer
reifyNat n = case someNatVal n of
  Just (SomeNat (_ :: Proxy m)) -> Just (fromInteger (natVal (Proxy @m)))
  Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Part D. 길이가 타입에 적힌 벡터 (Vec)
--
--   배경:
--     타입 수준 프로그래밍의 가장 실용적인 응용이다. 길이가 타입에 있으면:
--       * 빈 리스트에 head 를 줄 수 없다 (컴파일 오류)
--       * append 의 결과 길이가 자동으로 계산된다
--       * 행렬 곱셈의 차원 검사가 타입으로 된다
-- ---------------------------------------------------------------------------

data Vec (n :: Nat) a where
  VNil :: Vec 0 a
  VCons :: a -> Vec n a -> Vec (n + 1) a

instance (Show a) => Show (Vec n a) where
  show VNil = "VNil"
  show (VCons x xs) = "VCons " ++ show x ++ " " ++ show xs

-- ---------------------------------------------------------------------------
-- TODO 4. Vec 연산
--
--   목표:
--     a) appendV :: Vec n a -> Vec m a -> Vec (n + m) a
--     b) headV   :: Vec (n + 1) a -> a        -- 빈 벡터는 타입 자체가 불가능
--     c) vlengthV :: Vec n a -> Int          (값 재귀)
--     d) vlengthT 는 주어진 코드 (KnownNat n).
--     e) zipV    :: Vec n a -> Vec n b -> Vec n (a, b)   -- 길이 동치를 타입이 보장
--
--   힌트:
--     * appendV: VNil ys = ys;  VCons x xs ys = VCons x (appendV xs ys)
--       재귀 케이스에서 GHC 가 `n'+1+m ~ (n'+1)+m` 를 풀어야 한다.
--       보통 그대로 통과한다. 안 풀리면 패턴에서 n 을 k+1 로 드러내는
--       보조 정의를 써 봐라.
--     * headV (VCons x _) = x  -- VNil 은 타입이 불가능하므로 다룰 필요가 없다!
--       이게 "불가능한 상태를 타입으로 제거" 의 전형적인 예다.
--     * zipV: VNil VNil = VNil;  VCons x xs `zipV` VCons y ys = VCons (x,y) (...)
--       양쪽이 동시에 끝난다는 걸 타입이 보장한다.
-- ---------------------------------------------------------------------------

-- | 두 벡터를 이어붙인다. 결과 길이는 타입에서 (n + m) 으로 계산된다.
--
--   왜 단순한 재귀로 끝나지 않는가:
--     VCons x xs 에서 xs :: Vec n' a 이고 n ~ (n' + 1) 이다.
--     재귀 결과는 Vec ((n' + m) + 1) a 이지만, 우리가 약속한 타입은
--     Vec ((n' + 1) + m) a 다. 둘은 같은 수이지만
--     타입 가족 + 는 비단사(non-injective)라 n' 과 m 이 아직 변수일 때
--     GHC 가 이 결합법칙을 정규화하지 못한다. 그래서 다음이 풀리지 않는다.
--
--         (n' + m) + 1  ~  (n' + 1) + m
--
--     coerce 로도 해결되지 않는다. GADT 로 선언된 Vec 의 길이 인자 n 은
--     role 이 Nominal 로 추론되므로(값이 아닌 타입 인자라도) coerce 는
--     두 타입이 *문자 그대로 같음* 을 요구한다.
--
--   그래서 쓰는 것이 unsafeCoerce 다. 이곳에서 unsafe 인 이유는 타입뿐이다:
--     * 런타임 항(term)은 VCons x (appendV xs ys) 그대로이며 재해석되지 않는다.
--     * 양쪽 타입은 같은 값의 서로 다른 "길이 표기" 일 뿐이다.
--     * unsafeCoerce 는 이 타입 동치가 참이라는 사실에 대한 신뢰를 대신한다.
--   타입 안전한 대안은 길이를 Nat 이 아니라 타입 레벨 리스트('[] / ':)로
--   매겨서 타입 가족 ++ 가 첫 인자의 머리에서 바로 정규화되게 하는 것이다.
--   (그 경우 appendV 는 coerce 없이 그대로 타이핑된다.)
appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV VNil ys = ys
appendV (VCons x xs) ys = unsafeCoerce (VCons x (appendV xs ys))

headV :: Vec (n + 1) a -> a
headV (VCons x _) = x

-- | 길이를 값 레벨에서 센다. 결과는 Int 이므로 타입에 아무 정보도 주지 않는다.
--   타입 레벨 길이는 vlengthT 가 이미 알고 있다(KnownNat n).
vlengthV :: Vec n a -> Int
vlengthV VNil = 0
vlengthV (VCons _ xs) = 1 + vlengthV xs

vlengthT :: forall n a. (KnownNat n) => Vec n a -> Int
vlengthT _ = fromInteger (natVal (Proxy @n))

-- | 길이가 같은 두 벡터를 짝짓는다. 길이 불일치는 타입에서 거부된다.
--
--   왜 unsafeCoerce 가 필요한가:
--     VCons x xs 에서 xs :: Vec n' a, VCons y ys 에서 ys :: Vec n'' b 이고
--     양쪽 모두 n ~ (n' + 1), n ~ (n'' + 1) 이다. 즉 n' 과 n'' 은 같은 수지만,
--     타입 가족 + 가 비단사라 GHC 는 n' ~ n'' 을 스스로 끌어내지 못한다.
--     재귀 호출 zipV xs ys 는 두 인자의 길이 인자가 같아야 하므로
--     한쪽(여기서는 ys)을 Vec n' b 로 재표기한다.
--     런타임 항은 그대로이며, 양변은 같은 값의 다른 길이 표기일 뿐이다.
zipV :: Vec n a -> Vec n b -> Vec n (a, b)
zipV VNil VNil = VNil
zipV (VCons x xs) (VCons y ys) = VCons (x, y) (zipV xs (unsafeCoerce ys))

-- ---------------------------------------------------------------------------
-- TODO 5. (사고 실험 / 다음 단계)
--
--   a) `type family` 대신 `class` + `instance` (열린 타입 가족)은 언제 쓰는가?
--      힌트: 사용자가 인스턴스를 추가할 수 있어야 할 때.
--      예: class Container c where type Elem c;  Map k v 은 k, [] 은 a.
--   b) GHC.TypeLits 의 TypeError / Assert 을 써서 "좋은 오류 메시지" 를
--      만드는 기법을 찾아봐라(제약에 오류 메시지를 담는다).
--   c) singletons 라이브러리는 이 모든 것을 자동화한다. 한번 훑어봐라.
--   d) 타입 레벨 계산이 "값 레벨과 완전히 분리" 돼 있다는 사실의 의미:
--      타입은 컴파일 후 사라진다(erasable). 런타임 비용이 0 이라는 뜻이다.
--      그래서 "검사는 타입으로, 실행은 값으로" 하는 게 Haskell 의 방식이다.
--   e) Vec 대신 길이가 2개인 행렬(Matrix n m a)을 만들고, 곱셈의 차원 검사를
--      타입으로 해 봐라. appendV 와 같은 패턴이 반복되는지 관찰해 봐라.
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Ex10: Type Families / DataKinds / TypeLits ==="

  -- Part A: 타입 레벨 계산은 타입 애플리케이션으로 확인한다.
  print (natVal (Proxy @(If 'True (1 :: Nat) (2 :: Nat)))) -- 1
  print (natVal (Proxy @(If 'False (1 :: Nat) (2 :: Nat)))) -- 2
  print (natVal (Proxy @(If (Not 'True) (1 :: Nat) (0 :: Nat)))) -- 0
  print (natVal (Proxy @(If (And 'True 'False) (1 :: Nat) (0 :: Nat)))) -- 0

  -- Part B
  let xs = 1 :> "hello" :> HNil :: HList '[Int, String]
      ys = True :> HNil :: HList '[Bool]
  print (hLengthV xs) -- 2
  print (hLengthT xs) -- 2
  print (hLengthV (hAppend xs ys)) -- 3

  -- Part C
  print factorial5 -- 120
  print (natVal (Proxy @(Max 3 7))) -- 7
  print (reifyNat 5) -- Just 5
  print (reifyNat (-1)) -- Nothing

  -- Part D
  let v3 = VCons 10 (VCons 20 (VCons 30 VNil)) :: Vec 3 Int
      v2 = VCons 1 (VCons 2 VNil) :: Vec 2 Int
  print (vlengthV v3) -- 3
  print (vlengthT v3) -- 3
  print (headV (VCons 99 VNil :: Vec 1 Int)) -- 99
  print (vlengthV (appendV v2 v3)) -- 5
  -- zipV 는 길이가 같은 두 벡터만 받는다. 아래 줄을 v3 으로 바꾸면 컴파일 오류난다.
  -- (그게 바로 타입이 보장하는 것: 길이 불일치를 실행 오류가 아니라 컴파일 오류로)
  let w2 = VCons 10 (VCons 20 VNil) :: Vec 2 Int
  print (zipV v2 w2) -- VCons (1,10) (VCons (2,20) VNil)
  putStrLn "TODO 1~4 를 채우면 위 출력이 주석과 같아야 한다."
