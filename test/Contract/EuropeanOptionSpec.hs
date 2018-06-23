module Contract.EuropeanOptionSpec where

import Test.Hspec
import Contract.Contract as C
import Contract.Valuation
import Contract.FixedFXAndRatesModel

import Data.Function

import Prelude as P
import Data.Maybe

spec :: Spec
spec =
    describe "European option test" $
      it "Wilmott example" $
          wilmottOptJarrowPrice `shouldSatisfy` (isEqual 6.4496 . head . head)

isEqual :: Double -> Double -> Bool
isEqual x y = abs (x-y) < 0.0001

----------------- WILMOTT EXAMPLE ---------------------------------------------------------
--Example from Paul Wilmott introduces Quntitative Finance 2nd, p 80, 3.17 Valuing back down the tree
-- S=100, dT=1/12, r=0.1, Sigma=0.2; using continious compounding
-- params given in the book u=1.0604,v=0.9431, p'=0.5567 based on Cox, Ross, and Rubinstein

wilmott1stockObs :: Obs Double
wilmott1stockObs = Obs (const $ PR $ binomialTreeWithMult 100 1.0604 0.9431 5)

wilmott1stockObsJR :: Obs Double
wilmott1stockObsJR = Obs (const $ PR $ binomialTreeWithMult 100 pUp pDown 5)
    where
    pUp = pUpJarrowRudd 0.1 0.2 (1/12)      --1.066520727262422
    pDown = pDownJarrowRudd 0.1 0.2 (1/12)  --0.9502137114630924


wilmott1callContract :: Contract
wilmott1callContract = europeanCall wilmott1stockObs (konst 100) (mkDate 4)
wilmott1callContractJarrow :: Contract
wilmott1callContractJarrow = europeanCall wilmott1stockObsJR (konst 100) (mkDate 4) -- if exp(cont) compounding 6.449634745214092 or 6.359718808524029 if increase num of iters


wilmottOptPrice :: [RV Double]
wilmottOptPrice = take 15 $ unPr $ evalC wilmottModel USD wilmott1callContract
wilmottOptJarrowPrice :: [RV Double]
wilmottOptJarrowPrice = take 15 $ unPr $ evalC wilmottModel USD wilmott1callContractJarrow


staticModel :: Model
staticModel = fixedFXAndRatesModel()

wilmottModel :: Model
wilmottModel = staticModel {
    rateModel = rateModel',
    disc = disc'

}

rateModel' :: Currency -> PR Double
rateModel' k = fromMaybe (error $ "rateModel: currency not found " ++ show k)
        (lookup k rateModels)

rateModels :: [(Currency, PR Double)]
rateModels = [(USD, rates 0.1 0)]

rates :: Double -> Double -> PR Double
rates rateNow delta = PR $ makeRateSlices rateNow 1
  where
    makeRateSlices rateNow n = rateSlice rateNow n : makeRateSlices (rateNow-delta) (n+1)
    rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

disc' k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel' k)
  where
    discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
    discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
      if P.and bRv -- test for horizon
        then [pRv]
        else let rest@(nextSlice:_) = discCalc bs ps rs
                 discSlice = zipWith (\x r -> x * exp(-r*(1/12))) (prevSlice nextSlice) rateRv
                 thisSlice = zipWith3 (\b p q -> if b then p else q)
                               bRv pRv discSlice
             in thisSlice : rest

    prevSlice :: RV Double -> RV Double
    prevSlice [] = []
    prevSlice [_] = []
    prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest


------------------------------------------------------------------
pUpJarrowRudd,pDownJarrowRudd::Double->Double->Double->Double
pUpJarrowRudd   r stdev dt = exp((r-stdev*stdev/2)*dt+stdev*sqrt dt)
pDownJarrowRudd r stdev dt = exp((r-stdev*stdev/2)*dt-stdev*sqrt dt)

binomialTreeWithMult :: Double -> Double -> Double -> Int -> [[Double]]
binomialTreeWithMult start up down step = genTree [[start]] (step-1)
    where
        genTree :: [[Double]] -> Int -> [[Double]]
        genTree xs 0 = xs
        genTree xs step = let lastE = last xs in genTree (xs ++ [genStep lastE]) (step -1)
        genStep :: [Double] -> [Double]
        genStep lastAr = map (*up) lastAr ++ [last lastAr * down]


------------------------------------------------------------------------------
usd :: Obs Double -> Contract
usd o = scale o (one USD)

europeanCall :: Obs Double -> Obs Double -> Date -> Contract
europeanCall u strike d = when (at d) ((usd u `C.and` give (usd strike)) `C.or` zero)