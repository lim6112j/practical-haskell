module Functors where
import qualified Data.Map as M
import qualified Data.Tree as T

data TravelGuide = TravelGuide {
    title :: String,
    authors :: [String],
    price :: Double
} deriving (Show, Eq, Ord)

modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m = map (\tg -> tg { price = m * price tg})

modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
modifyTravelGuidePriceMap m = M.map (\tg -> tg { price = m * price tg})

modifyTravelGuidePriceTree :: Double -> T.Tree TravelGuide -> T.Tree TravelGuide
modifyTravelGuidePriceTree m = fmap (\tg -> tg { price = m * price tg })

-- map :: (a -> b) -> ([a] -> [b])
-- M.map :: (a -> b) -> (M.Map a -> M.Map b)
-- fmap :: (a -> b) -> (T.Tree a -> T.Tree b)
--
--

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { price = m * price tg})
