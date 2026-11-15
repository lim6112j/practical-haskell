{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Dsl
  ( main
  , v
  , noOffer
  , Offer (..)
  , Expr (..)
  , Leaf (..)
  , interpretExpr
  , interpretOffer
  , period
  , allOf
  , today
  , fromDay
  , untilDay
  , extend
  , restrict
  , Env (..)
  , leaf
  , lit
  , amountOf
  , priceOf
  , totalNumberProducts
  , totalPrice
  , onlyProducts
  , Product (..)
  , makerOf
  , producedOnOf
  , useByOf
  , ageInDays
  , daysUntilExpiry
  ) where

import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

{- | The environment an expression is read against: the day the evaluation
happens on, plus the basket, holding one @(product, unit price)@ pair per unit.
This is what 'interpretExpr' and 'interpretOffer' read, and it is the whole of
the context a leaf can see.

Both halves live here rather than in the interpreter's parameter list, because a
leaf may need either of them: 'ageInDays' needs the day, 'totalPrice' needs the
basket. Keeping them in one place is what lets every attribute stay a plain
function, with no day threaded through by hand.

This is the only place where the products and their attributes live. The core
below never mentions a product field, so giving the product record one more
field is not a change to the DSL: it is a new function, and nothing else.
-}
data Env a = Env
  { envToday :: Day
  , envBasket :: [(a, Float)]
  }
  deriving (Show)

{- | A leaf: a primitive value read out of the environment.

Leaves are the only part of the core that knows anything about the products,
and they are plain functions rather than constructors, so a new attribute is a
new function and the core does not grow. The wrapper exists so that 'Expr' and
'Offer' can still derive 'Show': a function cannot be shown, only a placeholder
for one.
-}
newtype Leaf e r = Leaf (e -> r)

instance Show (Leaf e r) where
  show _ = "<leaf>"

{- | The offer core: the shapes a discount can take, and the ways discounts
combine.

Nothing else survives here. A guard around an offer -- a time window, a unit
count, a restriction on what the basket may hold -- is only ever a condition,
and conditions live in 'Expr', so 'fromDay', 'untilDay', 'extend' and
'restrict' below are ordinary functions. What is left is what the core has to
own: the arithmetic of a discount, and the three ways two offers are put
together.
-}
data Offer a where
  Present :: a -> Offer a
  PercentDiscount :: Float -> Offer a
  AbsoluteDiscount :: Float -> Offer a
  Both :: (Offer a) -> (Offer a) -> Offer a
  BetterOf :: (Offer a) -> (Offer a) -> Offer a
  If :: (Expr a Bool) -> (Offer a) -> (Offer a) -> Offer a

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

{- | The core. Every attribute of every product is reached through the single
leaf constructor 'Prim'; every other constructor here only combines
expressions and never mentions a product.

That is why the core stays this small no matter how many attributes the
products turn out to have: an attribute is a function, and functions need no
constructor. Only a genuinely new /shape/ of combination -- "the better of
these two", "if this then that" -- costs a constructor here.
-}
data Expr a r where
  Prim :: String -> Leaf (Env a) r -> Expr a r
  (:+:) :: (Num n) => (Expr a n) -> (Expr a n) -> Expr a n
  (:*:) :: (Num n) => (Expr a n) -> (Expr a n) -> Expr a n
  (:<:) :: (Ord n) => (Expr a n) -> (Expr a n) -> Expr a Bool
  (:<=:) :: (Ord n) => (Expr a n) -> (Expr a n) -> Expr a Bool
  (:>:) :: (Ord n) => (Expr a n) -> (Expr a n) -> Expr a Bool
  (:>=:) :: (Ord n) => (Expr a n) -> (Expr a n) -> Expr a Bool
  (:==:) :: (Eq n) => (Expr a n) -> (Expr a n) -> Expr a Bool
  (:&&:) :: (Expr a Bool) -> (Expr a Bool) -> Expr a Bool
  (:||:) :: (Expr a Bool) -> (Expr a Bool) -> Expr a Bool
  Not :: (Expr a Bool) -> Expr a Bool

deriving instance (Show a) => Show (Expr a r)

deriving instance (Show a, Show (Expr a Bool)) => Show (Offer a)

-- | Builds a leaf. The name is only ever shown; the function is what runs.
leaf :: String -> (Env a -> r) -> Expr a r
leaf name = Prim name . Leaf

{- | A literal of any type, so that numbers, days and texts all read the same.
One constructor per literal type would grow the core by one per type; a
function grows it by none.
-}
lit :: r -> Expr a r
lit = leaf "lit" . const

-- | How many units of this product the basket holds.
amountOf :: (Eq a) => a -> Expr a Integer
amountOf p =
  leaf "amountOf" (\env -> fromIntegral (length (filter ((== p) . fst) (envBasket env))))

-- | The unit price of this product, or 0 when the basket holds none.
priceOf :: (Eq a) => a -> Expr a Float
priceOf p = leaf "priceOf" (\env -> maybe 0.0 id (lookup p (envBasket env)))

-- | The number of units in the basket.
totalNumberProducts :: Expr a Integer
totalNumberProducts = leaf "totalNumberProducts" (fromIntegral . length . envBasket)

-- | What the whole basket is worth.
totalPrice :: Expr a Float
totalPrice = leaf "totalPrice" (sum . map snd . envBasket)

{- | The day the environment was built for.

With the day reachable as an ordinary leaf, a time window is an ordinary
condition, which is what lets 'fromDay' and 'untilDay' be plain functions
instead of constructors of 'Offer'.
-}
today :: Expr a Day
today = leaf "today" envToday

-- | Whether every unit in the basket is one of these products.
onlyProducts :: (Eq a) => [a] -> Expr a Bool
onlyProducts ps =
  leaf "onlyProducts" (\env -> all (`elem` ps) (map fst (envBasket env)))

{- | A product, as far as the offers are concerned. None of these fields is
known to the core: 'Expr' only ever sees the 'Product' values that the
environment happens to hold. Adding a field here adds no constructor, no
interpreter clause and no export to the DSL -- only one more function of the
same shape as the ones below.
-}
data Product = Product
  { productName :: String
  , productMaker :: String
  , producedOn :: Day
  , useBy :: Day
  }
  deriving (Show, Eq)

-- | Who made it.
makerOf :: Product -> Expr Product String
makerOf p = leaf "makerOf" (\_ -> productMaker p)

-- | When it was made.
producedOnOf :: Product -> Expr Product Day
producedOnOf p = leaf "producedOnOf" (\_ -> producedOn p)

-- | When it stops being good.
useByOf :: Product -> Expr Product Day
useByOf p = leaf "useByOf" (\_ -> useBy p)

{- | How old it is on the day the environment was built for; 0 when it was made
in the future.

A derived attribute is a leaf like any other, which is why there is no
subtraction on days in the core. The day is read from the environment, so the
expression itself never carries one.
-}
ageInDays :: Product -> Expr Product Integer
ageInDays p = leaf "ageInDays" (\env -> max 0 (diffDays (envToday env) (producedOn p)))

-- | Days left before it expires, 0 once it has.
daysUntilExpiry :: Product -> Expr Product Integer
daysUntilExpiry p =
  leaf "daysUntilExpiry" (\env -> max 0 (diffDays (useBy p) (envToday env)))

interpretExpr :: Expr a t -> Env a -> t
interpretExpr (Prim _ (Leaf f)) env =
  f env
interpretExpr (e1 :+: e2) env =
  interpretExpr e1 env + interpretExpr e2 env
interpretExpr (e1 :*: e2) env =
  interpretExpr e1 env * interpretExpr e2 env
interpretExpr (e1 :<: e2) env =
  interpretExpr e1 env < interpretExpr e2 env
interpretExpr (e1 :<=: e2) env =
  interpretExpr e1 env <= interpretExpr e2 env
interpretExpr (e1 :>: e2) env =
  interpretExpr e1 env > interpretExpr e2 env
interpretExpr (e1 :>=: e2) env =
  interpretExpr e1 env >= interpretExpr e2 env
interpretExpr (e1 :==: e2) env =
  interpretExpr e1 env == interpretExpr e2 env
interpretExpr (e1 :&&: e2) env =
  interpretExpr e1 env && interpretExpr e2 env
interpretExpr (e1 :||: e2) env =
  interpretExpr e1 env || interpretExpr e2 env
interpretExpr (Not e) env =
  not (interpretExpr e env)

{- |
Interprets an offer as the amount of money that is taken off of the environment
it is read against: the day the evaluation happens on, plus a basket holding
one @(product, unit price)@ pair per unit, which is what 'interpretExpr'
expects.

Nothing here guards on the day or on the basket: 'fromDay', 'untilDay',
'extend' and 'restrict' are functions built out of 'If' and a leaf, so this
interpreter only has to know what a discount is worth and how two offers
combine. That is also why it needs no constraint on the product type.
-}
interpretOffer :: Offer a -> Env a -> Float
interpretOffer (Present _) _ =
  0
interpretOffer (PercentDiscount percent) env =
  interpretExpr totalPrice env * percent / 100
interpretOffer (AbsoluteDiscount amount) _ =
  amount
interpretOffer (Both offer1 offer2) env =
  interpretOffer offer1 env + interpretOffer offer2 env
interpretOffer (BetterOf offer1 offer2) env =
  max (interpretOffer offer1 env) (interpretOffer offer2 env)
interpretOffer (If cond offer1 offer2) env =
  if interpretExpr cond env
    then interpretOffer offer1 env
    else interpretOffer offer2 env

{- |
Applies the offer from the given day onwards.

This used to be a constructor of 'Offer'. It is a function now, because a time
window is nothing but a condition on the day, and conditions already live in
'Expr'. A guard that fails contributes 'noOffer', which is exactly what the old
constructor contributed, so nothing about the discounts changes.
-}
fromDay :: Day -> Offer a -> Offer a
fromDay d o = If (today :>=: lit d) o noOffer

-- | Applies the offer up to and including the given day.
untilDay :: Day -> Offer a -> Offer a
untilDay d o = If (today :<=: lit d) o noOffer

-- | Applies the offer once the basket grows beyond @n@ units.
extend :: Integer -> Offer a -> Offer a
extend n o = If (totalNumberProducts :>: lit n) o noOffer

{- | A flat amount off, but only when every unit in the basket is one of these
products.

This used to be a constructor too. It is a function now for the same reason as
'fromDay': what "restrict" means -- every unit, or at least one, or none -- is a
business rule, and a rule that lives in the interpreter can only be changed by
changing the core. As a function it lives in the library, where it can.
-}
restrict :: (Eq a) => [a] -> Float -> Offer a
restrict ps amount = If (onlyProducts ps) (AbsoluteDiscount amount) noOffer

{- |
Constrains an offer to a window of @d@ days that starts at day @f@, that is, to
every day from @f@ up to and including @f + d@.

'fromDay' and 'untilDay' take points in time rather than lengths, so the length
@d@ cannot be handed to them directly: it has to be turned into the closing day
of the window first. The two guards are nested rather than combined with 'Both',
because 'Both' adds up the discounts of both branches and would therefore apply
@o@ twice on every day inside the window.
-}
period :: Day -> Integer -> Offer a -> Offer a
period f d o = fromDay f (untilDay (addDays d f) o)

{- |
Conjunction of a list of offers: every one of them has to apply, and their
discounts add up. The empty list applies no discount at all.
-}
allOf :: [Offer a] -> Offer a
allOf = foldr Both noOffer

-- | Ten units off, or a free balloon plus 5% off once the basket is worth more
-- than 100, whichever is best, for baskets of at most 30 units. 'Until' now
-- takes a point in time, so the unit cap is spelled out with a condition.
v :: Offer String
v =
  If
    (totalNumberProducts :<=: lit 30)
    (BetterOf
      (AbsoluteDiscount 10.0)
      (Both (Present "balloon") (If (totalPrice :>: lit 100) (PercentDiscount 5.0) noOffer)))
    noOffer

-- | 20% off from 27 November 2026 until 3 December 2026, both days included.
blackFridayWeek :: Offer String
blackFridayWeek = period (fromGregorian 2026 11 27) 6 (PercentDiscount 20.0)

-- | The basket the string offers are read against, on the given day.
basket :: Day -> Env String
basket day =
  Env day [("wine", 60.0), ("cheese", 30.0), ("bread", 15.0), ("wine", 60.0), ("wine", 60.0)]

cheese :: Product
cheese = Product "Aged Cheese" "Nonghyup" (fromGregorian 2026 11 5) (fromGregorian 2026 12 20)

wine :: Product
wine = Product "Red Wine" "Chateau X" (fromGregorian 2025 3 1) (fromGregorian 2033 6 1)

productBasket :: Day -> Env Product
productBasket day = Env day [(cheese, 20000.0), (wine, 60000.0), (cheese, 20000.0)]

{- | A policy over four attributes the core has never heard of: who made it,
when it was made, when it expires and how old it is. 20% off Korean cheese
that is at most ten days old and still has a week on it, or 5,000 off,
whichever is better; otherwise 10% off cheese that is within a week of
expiring.

Note that nothing above 'ageInDays' was touched to write this: the core gained
no constructor and the interpreter gained no clause. It does not even take a
day -- that comes from the environment, so the same policy can be read against
any day.
-}
policy :: Offer Product
policy =
  If
    ( (makerOf cheese :==: lit "Nonghyup")
        :&&: (ageInDays cheese :<=: lit 10)
        :&&: (daysUntilExpiry cheese :>=: lit 7)
    )
    (BetterOf (PercentDiscount 20.0) (AbsoluteDiscount 5000.0))
    (If (daysUntilExpiry cheese :<: lit 7) (PercentDiscount 10.0) noOffer)

-- | A flat 10 off when the basket holds nothing but these three, plus 5% off
-- once it grows past three units.
stringPolicy :: Offer String
stringPolicy =
  Both
    (restrict ["wine", "cheese", "bread"] 10.0)
    (extend 3 (PercentDiscount 5.0))

main :: IO ()
main = do
  now <- utctDay <$> getCurrentTime
  print $ "totalnumber " <> show (interpretExpr totalNumberProducts (basket now))
  print $ "offer v " <> show (interpretOffer v (basket now))
  print $
    "offer allOf [10 off, 5%] "
      <> show (interpretOffer (allOf [AbsoluteDiscount 10.0, PercentDiscount 5.0]) (basket now))
  print $ "offer allOf [] " <> show (interpretOffer (allOf [] :: Offer String) (basket now))
  print $ "offer stringPolicy " <> show (interpretOffer stringPolicy (basket now))
  mapM_
    ( \day ->
        print $
          "offer blackFridayWeek on "
            <> show day
            <> " -> "
            <> show (interpretOffer blackFridayWeek (basket day))
    )
    [ fromGregorian 2026 11 26 -- the day before the window opens
    , fromGregorian 2026 11 27 -- the first day of the window
    , fromGregorian 2026 12 3 -- the last day of the window
    , fromGregorian 2026 12 4 -- the day after the window closes
    ]
  mapM_
    ( \day -> do
        print $
          "cheese on "
            <> show day
            <> ": made by "
            <> show (interpretExpr (makerOf cheese) (productBasket day))
            <> " on "
            <> show (interpretExpr (producedOnOf cheese) (productBasket day))
            <> ", age "
            <> show (interpretExpr (ageInDays cheese) (productBasket day))
            <> " days, "
            <> show (interpretExpr (daysUntilExpiry cheese) (productBasket day))
            <> " days to expiry"
        print $ "  policy -> " <> show (interpretOffer policy (productBasket day))
    )
    [ fromGregorian 2026 11 8 -- three days after it was made
    , fromGregorian 2026 11 20 -- too old for the first branch
    , fromGregorian 2026 12 15 -- a week from expiring
    ]
  print $ "policy tree " <> show (policy :: Offer Product)
