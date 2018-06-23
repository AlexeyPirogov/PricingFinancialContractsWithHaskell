module Contract.FixedFXAndRatesModelSpec (spec) where

import Test.Hspec
import Contract.FixedFXAndRatesModel
import Contract.Contract as C
import Contract.Valuation

spec :: Spec
spec =
  describe "Fix FX and rates model test" $ do
    it "echange test" $
        simpleEval fxForwardTodayC `shouldBe` ([[150]] :: [[Double]])
    it "discount test" $ do
        simpleEval zcbTodayC `shouldBe` ([[100]] :: [[Double]])
        simpleEval zcbNextDateC `shouldBe` ([[100/1.01],[100,100]] :: [[Double]])
        simpleEval zcbNext2DateC `shouldBe`
          ([[(100/1.01)/1.01],[100/1.01,100/1.01],[100,100,100]] :: [[Double]])
    it "and, give, exch test" $ do
        simpleEval fxForwardZeroSumC `shouldStartWith` ([[0]] :: [[Double]])
        simpleEval fxForwardZeroSumDate1C `shouldStartWith` ([[0]] :: [[Double]])
        simpleEval fxSwapC `shouldSatisfy` (isZero . head . head)

isZero :: Double -> Bool
isZero n = n < 0.00000001 && n > 0

date0, date1, date2, date3, date4 :: Date
date0 = mkDate 0
date1 = mkDate 1
date2 = mkDate 2
date3 = mkDate 3
date4 = mkDate 4

simpleEval :: Contract -> [RV Double]
simpleEval c = take 5 $ unPr $ evalC (fixedFXAndRatesModel()) USD c
------------------
--exch test
fxForwardTodayC :: Contract
fxForwardTodayC = zcb date0 100 EUR
--disc test
zcbTodayC :: Contract
zcbTodayC = zcb date0 100 USD
zcbNextDateC :: Contract
zcbNextDateC = zcb date1 100 USD
zcbNext2DateC :: Contract
zcbNext2DateC = zcb date2 100 USD

--and, give, exch test
fxForwardZeroSumC :: Contract
fxForwardZeroSumC = zcb date0 150 USD `C.and` give (zcb date0 100 EUR)
fxForwardZeroSumDate1C :: Contract
fxForwardZeroSumDate1C = zcb date1 150 USD `C.and` give (zcb date1 100 EUR)

fxSwapC :: Contract
fxSwapC = zcb date1 150 USD
    `C.and` zcb date2 150 USD
    `C.and` zcb date3 150 USD
    `C.and` zcb date4 150 USD
    `C.and` give (zcb date1 100 EUR)
    `C.and` give (zcb date2 100 EUR)
    `C.and` give (zcb date3 100 EUR)
    `C.and` give (zcb date4 100 EUR)