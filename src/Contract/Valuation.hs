module Contract.Valuation
(evalC,
Model (..)
) where

import Control.Applicative(liftA2, liftA3)
import Contract.Contract

data Model = Model {
   modelStart :: Date,
   disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
   exch       :: Currency -> Currency -> PR Double,
   absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
   rateModel  :: Currency -> PR Double
}

evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval
  where eval Zero           = bigK 0
        eval (One k2)       = exch k2 k
        eval (Give c)       = -(eval c)
        eval (o `Scale` c)  = evalO o * eval c
        eval (c1 `And` c2)  = eval c1 + eval c2
        eval (c1 `Or` c2)   = max (eval c1) (eval c2)
        eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
        eval (When o c)     = disc   k (evalO o, eval c)
        eval (Until o c)    = absorb k (evalO o, eval c)

evalO :: Obs a -> PR a
evalO (Obs o) = o time0

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = liftA3 (\b tru fal -> if b then tru else fal)

instance Eq a => Eq (PR a) where
  (PR a) == (PR b) = a == b

instance Ord a => Ord (PR a) where
  max = liftA2 max

instance Num a => Num (PR a) where
  fromInteger i = bigK (fromInteger i)
  (+) = lift2PrAll (+)
  (-) = lift2PrAll (-)
  (*) = lift2PrAll (*)
  abs = fmap  abs
  signum = fmap signum

lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll _ as@(_:_) []       = as
zipWithAll _ []       bs@(_:_) = bs
zipWithAll _ _        _        = []