{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dsl (
  main,
  v,
  noOffer,
  Offer (..),
  Expr (..),
  interpretExpr,
  interpretOffer,
  Product (..),
  ProductKind (..),
  expiryDateOf,
  requiresColdChain,
  warrantyOf,
  Basket (..),
  isFood,
  isManufactured,
  isExpired,
  ageInDays,
  daysUntilExpiry,
  expiresWithin,
  warrantyMonthsOf,
) where

import Data.List (find)
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

{- | The kind of goods a 'Product' is. Food carries a shelf life and a cold
chain flag, manufactured goods a warranty length, so the two families of
attributes are kept apart instead of being flattened into one bag of optional
fields. Use 'expiryDateOf', 'requiresColdChain' and 'warrantyOf' to read them
back without a partial pattern match.
-}
data ProductKind
  = -- | @Food expiryDate requiresColdChain@
    Food Day Bool
  | -- | @Manufactured warrantyMonths@
    Manufactured Integer
  deriving (Eq, Show)

-- | Expiry date of a food product; 'Nothing' for manufactured goods.
expiryDateOf :: ProductKind -> Maybe Day
expiryDateOf (Food d _) = Just d
expiryDateOf Manufactured{} = Nothing

-- | Whether the goods must be kept cold; only food can.
requiresColdChain :: ProductKind -> Bool
requiresColdChain (Food _ cold) = cold
requiresColdChain Manufactured{} = False

-- | Length of the warranty of a manufactured product, in months; 0 for food.
warrantyOf :: ProductKind -> Integer
warrantyOf (Manufactured m) = m
warrantyOf Food{} = 0

{- | One unit of goods. A basket holds one entry per unit, so the unit price
lives here rather than in a separate @(product, price)@ pair.
-}
data Product = Product
  { productId :: String
  , productName :: String
  , unitPrice :: Float
  , productionDate :: Day
  , productKind :: ProductKind
  }
  deriving (Show)

{- | Products are identified by 'productId' alone, so the same goods may appear
several times in a basket at different unit prices (for instance once
discounted).
-}
instance Eq Product where
  p == p' = productId p == productId p'

{- | The basket an offer is evaluated against: the goods themselves, plus the
day the evaluation happens on, which shelf-life expressions need.
-}
data Basket = Basket
  { basketToday :: Day
  , basketItems :: [Product]
  }
  deriving (Show)

isFood :: Product -> Bool
isFood p = case productKind p of
  Food{} -> True
  Manufactured{} -> False

isManufactured :: Product -> Bool
isManufactured = not . isFood

{- |
Whether a food product is past its expiry date on the given day. Manufactured
goods do not expire this way, so they are never expired.
-}
isExpired :: Day -> Product -> Bool
isExpired today p = case productKind p of
  Food d _ -> d < today
  Manufactured{} -> False

-- | Age of a product in days; 0 when it was produced in the future.
ageInDays :: Day -> Product -> Integer
ageInDays today p = max 0 (diffDays today (productionDate p))

{- |
Days left before a food product expires, 0 once it is expired. Manufactured
goods have no expiry date and report 0.
-}
daysUntilExpiry :: Day -> Product -> Integer
daysUntilExpiry today p = case productKind p of
  Food d _ -> max 0 (diffDays d today)
  Manufactured{} -> 0

-- | Length of the warranty of a manufactured product, in months. Food reports 0.
warrantyMonthsOf :: Product -> Integer
warrantyMonthsOf p = case productKind p of
  Manufactured m -> m
  Food{} -> 0

{- |
Whether a food product expires within @n@ days of the given day. Manufactured
goods have no expiry date, so they never expire within any window.
-}
expiresWithin :: Day -> Integer -> Product -> Bool
expiresWithin today n p =
  isFood p && daysUntilExpiry today p <= n

data Offer where
  Present :: Product -> Offer
  PercentDiscount :: Float -> Offer
  AbsoluteDiscount :: Float -> Offer
  Restrict :: [Product] -> Float -> Offer
  From :: Integer -> Offer -> Offer
  Until :: Integer -> Offer -> Offer
  Extend :: Integer -> Offer -> Offer
  Both :: Offer -> Offer -> Offer
  BetterOf :: Offer -> Offer -> Offer
  If :: (Expr Bool) -> Offer -> Offer -> Offer

noOffer :: Offer
noOffer = AbsoluteDiscount 0

data Expr r where
  AmountOf :: Product -> Expr Integer
  PriceOf :: Product -> Expr Float
  TotalNumberProducts :: Expr Integer
  TotalPrice :: Expr Float
  IsFood :: Product -> Expr Bool
  IsManufactured :: Product -> Expr Bool
  IsExpired :: Product -> Expr Bool
  AgeOf :: Product -> Expr Integer
  DaysUntilExpiry :: Product -> Expr Integer
  WarrantyOf :: Product -> Expr Integer
  AllExpireWithin :: Integer -> Expr Bool
  IVal :: Integer -> Expr Integer
  FVal :: Float -> Expr Float
  (:+:) :: (Num n) => (Expr n) -> (Expr n) -> Expr n
  (:*:) :: (Num n) => (Expr n) -> (Expr n) -> Expr n
  (:<:) :: (Num n, Ord n) => (Expr n) -> (Expr n) -> Expr Bool
  (:<=:) :: (Num n, Ord n) => (Expr n) -> (Expr n) -> Expr Bool
  (:>:) :: (Num n, Ord n) => (Expr n) -> (Expr n) -> Expr Bool
  (:>=:) :: (Num n, Ord n) => (Expr n) -> (Expr n) -> Expr Bool
  (:&&:) :: (Expr Bool) -> (Expr Bool) -> Expr Bool
  (:||:) :: (Expr Bool) -> (Expr Bool) -> Expr Bool
  Not :: (Expr Bool) -> Expr Bool

deriving instance Show (Expr r)

deriving instance Show Offer

interpretExpr :: Expr t -> Basket -> t
interpretExpr (AmountOf prod) Basket{..} =
  fromIntegral $ length [() | p <- basketItems, p == prod]
interpretExpr (PriceOf prod) Basket{..} =
  maybe 0.0 unitPrice (find (== prod) basketItems)
interpretExpr TotalNumberProducts Basket{..} =
  fromIntegral $ length basketItems
interpretExpr TotalPrice Basket{..} =
  sum (map unitPrice basketItems)
interpretExpr (IsFood prod) _ =
  isFood prod
interpretExpr (IsManufactured prod) _ =
  isManufactured prod
interpretExpr (IsExpired prod) Basket{..} =
  isExpired basketToday prod
interpretExpr (AgeOf prod) Basket{..} =
  ageInDays basketToday prod
interpretExpr (DaysUntilExpiry prod) Basket{..} =
  daysUntilExpiry basketToday prod
interpretExpr (WarrantyOf prod) _ =
  warrantyMonthsOf prod
interpretExpr (AllExpireWithin n) Basket{..} =
  all (expiresWithin basketToday n) basketItems
interpretExpr (IVal n) _ =
  fromInteger n
interpretExpr (FVal f) _ =
  f
interpretExpr (e1 :+: e2) b =
  interpretExpr e1 b + interpretExpr e2 b
interpretExpr (e1 :*: e2) b =
  interpretExpr e1 b * interpretExpr e2 b
interpretExpr (e1 :<: e2) b =
  interpretExpr e1 b < interpretExpr e2 b
interpretExpr (e1 :<=: e2) b =
  interpretExpr e1 b <= interpretExpr e2 b
interpretExpr (e1 :>: e2) b =
  interpretExpr e1 b > interpretExpr e2 b
interpretExpr (e1 :>=: e2) b =
  interpretExpr e1 b >= interpretExpr e2 b
interpretExpr (e1 :&&: e2) b =
  interpretExpr e1 b && interpretExpr e2 b
interpretExpr (e1 :||: e2) b =
  interpretExpr e1 b || interpretExpr e2 b
interpretExpr (Not e) b =
  not (interpretExpr e b)

{- |
Interprets an offer as the amount of money that is taken off a basket of
products. The basket holds one entry per unit of goods, which is what
'interpretExpr' expects.

The combinators guard an offer on the basket: 'From' applies from @n@ units
onwards, 'Extend' applies once the basket grows beyond @n@ units, and 'Until'
applies only while every unit in the basket expires within @n@ days -- a
manufactured good has no expiry date, so a basket holding one never qualifies.
-}
interpretOffer :: Offer -> Basket -> Float
interpretOffer (Present _) _ =
  0
interpretOffer (PercentDiscount percent) b =
  interpretExpr TotalPrice b * percent / 100
interpretOffer (AbsoluteDiscount amount) _ =
  amount
interpretOffer (Restrict prods amount) Basket{..} =
  if all (`elem` prods) basketItems then amount else 0
interpretOffer (From n offer) b =
  if interpretExpr (TotalNumberProducts :>=: IVal n) b
    then interpretOffer offer b
    else 0
interpretOffer (Until n offer) b =
  if interpretExpr (AllExpireWithin n) b
    then interpretOffer offer b
    else 0
interpretOffer (Extend n offer) b =
  if interpretExpr (TotalNumberProducts :>: IVal n) b
    then interpretOffer offer b
    else 0
interpretOffer (Both offer1 offer2) b =
  interpretOffer offer1 b + interpretOffer offer2 b
interpretOffer (BetterOf offer1 offer2) b =
  max (interpretOffer offer1 b) (interpretOffer offer2 b)
interpretOffer (If cond offer1 offer2) b =
  if interpretExpr cond b
    then interpretOffer offer1 b
    else interpretOffer offer2 b

balloon :: Product
balloon =
  Product
    { productId = "gift-001"
    , productName = "Balloon"
    , unitPrice = 0.0
    , productionDate = fromGregorian 2026 1 1
    , productKind = Manufactured 0
    }

wine :: Product
wine =
  Product
    { productId = "wine-001"
    , productName = "Red Wine"
    , unitPrice = 60.0
    , productionDate = fromGregorian 2025 3 1
    , productKind = Manufactured 24
    }

cheese :: Product
cheese =
  Product
    { productId = "cheese-001"
    , productName = "Aged Cheese"
    , unitPrice = 30.0
    , productionDate = fromGregorian 2026 10 1
    , productKind = Food (fromGregorian 2026 12 1) False
    }

bread :: Product
bread =
  Product
    { productId = "bread-001"
    , productName = "Sourdough Bread"
    , unitPrice = 15.0
    , productionDate = fromGregorian 2026 11 10
    , productKind = Food (fromGregorian 2026 11 13) False
    }

yoghurt :: Product
yoghurt =
  Product
    { productId = "yoghurt-001"
    , productName = "Plain Yoghurt"
    , unitPrice = 3.5
    , productionDate = fromGregorian 2026 11 5
    , productKind = Food (fromGregorian 2026 11 14) True
    }

{- | Ten units off, or a free balloon plus 5% off once the basket is worth more
than 100, whichever is better. 'Until' now guards on shelf life, so the
"at most 30 units" cap is spelled out with an explicit condition.
-}
v :: Offer
v =
  If
    (TotalNumberProducts :<=: IVal 30)
    ( BetterOf
        (AbsoluteDiscount 10.0)
        (Both (Present balloon) (If (TotalPrice :>: FVal 100) (PercentDiscount 5.0) noOffer))
    )
    noOffer

{- | Clearance on one kind of goods: 20% off yoghurt that expires within three
days of today.
-}
clearance :: Offer
clearance =
  If
    (DaysUntilExpiry yoghurt :<=: IVal 3)
    (PercentDiscount 20.0)
    noOffer

{- | Clearance on a whole basket: 20% off once every unit in it expires within
three days of today.
-}
clearanceBasket :: Offer
clearanceBasket = Until 3 (PercentDiscount 20.0)

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  let basket =
        Basket
          { basketToday = today
          , basketItems = [wine, cheese, bread, wine, wine, yoghurt]
          }
      expiring = Basket today [yoghurt, yoghurt, bread]
  print $ "totalnumber " <> show (interpretExpr TotalNumberProducts basket)
  print $ "totalprice " <> show (interpretExpr TotalPrice basket)
  print $ "bread age (days) " <> show (interpretExpr (AgeOf bread) basket)
  print $ "bread days until expiry " <> show (interpretExpr (DaysUntilExpiry bread) basket)
  print $ "bread expired? " <> show (interpretExpr (IsExpired bread) basket)
  print $ "wine warranty (months) " <> show (interpretExpr (WarrantyOf wine) basket)
  print $ "wine manufactured? " <> show (interpretExpr (IsManufactured wine) basket)
  print $
    "mixed basket: every unit expires within 3 days? "
      <> show (interpretExpr (AllExpireWithin 3) basket)
  print $
    "expiring basket: every unit expires within 3 days? "
      <> show (interpretExpr (AllExpireWithin 3) expiring)
  print $ "offer v " <> show (interpretOffer v basket)
  print $ "offer clearance (yoghurt within 3 days) " <> show (interpretOffer clearance basket)
  print $ "offer clearanceBasket on mixed basket " <> show (interpretOffer clearanceBasket basket)
  print $
    "offer clearanceBasket on expiring basket "
      <> show (interpretOffer clearanceBasket expiring)
