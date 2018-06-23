module Contract.FixedFXAndRatesModel (fixedFXAndRatesModel) where

import Contract.Valuation
import Contract.Contract as C

import Prelude as P
import Data.Maybe

fixedFXAndRatesModel :: CalendarTime -> Model
fixedFXAndRatesModel modelDate = Model {
  modelStart = (modelDate,0),
  disc       = disc',
  exch       = exch',
  absorb     = absorb',
  rateModel  = rateModel'
  }
  where
    rateModel' :: Currency -> PR Double
    rateModel' k =
      fromMaybe (error $ "rateModel: currency not found " ++ show k)
        (lookup k rateModels)

    rateModels :: [(Currency, PR Double)]
    rateModels = [(EUR, rates 1 0)
                ,(USD, rates 1   0)
                ,(GBP, rates 8   0.5)
                ,(CHF, rates 7   0.8)
                ]

    rates :: Double -> Double -> PR Double
    rates rateNow delta = PR $ makeRateSlices rateNow 1
      where
        makeRateSlices rateNow n = rateSlice rateNow n : makeRateSlices (rateNow-delta) (n+1)
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

    disc' :: Currency -> (PR Bool, PR Double) -> PR Double
    disc' k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel' k)
      where
        discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
        discCalc (bRv:bRvs) (pRv:ps) (rateRv:rateRvs) =
          if P.and bRv
            then [pRv]
            else let rest@(nextSlice:_) = discCalc bRvs ps rateRvs
                     discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                     thisSlice = zipWith3 (\b p q -> if b then p else q)
                                   bRv pRv discSlice
                 in thisSlice : rest

        prevSlice :: RV Double -> RV Double
        prevSlice [] = []
        prevSlice [_] = []
        prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

    absorb' :: Currency -> (PR Bool, PR Double) -> PR Double
    absorb' k (PR bSlices, PR rvs) =
      PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                   bSlices rvs

    exch' :: Currency -> Currency -> PR Double
    exch' k1 k2
         = fromMaybe (error $ "fxRateModel: currency pair not found " ++ show k1 ++ "/" ++ show k2)
                         (lookup (k1, k2) fxRateModels)

    fxRateModels :: [((Currency, Currency), PR Double)]
    fxRateModels = [((EUR, USD), fxRates 1.5 0)
                   ,((USD, EUR), fxRates 0.6667 0)
                   ,((GBP, USD), fxRates 1.3 0.3)
                   ,((USD, GBP), fxRates 0.769 0.3)
                   ,((USD, USD), fxRates 1 0)
                   ]

    fxRates :: Double -> Double -> PR Double
    fxRates rateNow delta = PR $ makeRateSlices rateNow 1
      where
        makeRateSlices rate n = rateSlice rate n : makeRateSlices (rate-delta) (n+1)
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]