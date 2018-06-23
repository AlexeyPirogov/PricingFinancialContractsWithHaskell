module Contract.Contract
 (when,
 PR (..),
 Obs (..),
 Contract (..),
 Currency (..),
 Date,
 bigK,
 time0,
 RV,
 CalendarTime,
 Contract.Contract.or,
 and,
 zero,
 give,
 at,
 one,
 scale,
 mkDate,
 konst,
 zcb,
 give
 )
 where

import Data.Time
import Prelude hiding (and)

import Control.Applicative(liftA2)

type Days = Double
type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)

data Contract =
      Zero
    | One  Currency
    | Give Contract
    | And  Contract Contract
    | Or   Contract Contract
    | Cond    (Obs Bool)   Contract Contract
    | Scale   (Obs Double) Contract
    | When    (Obs Bool)   Contract
    | Anytime (Obs Bool)   Contract
    | Until   (Obs Bool)   Contract
    deriving Show

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = when (at t) (scale (konst x) (one k))

andGive :: Contract -> Contract -> Contract
andGive c d = c `and` give d

scaleK :: Double -> Contract -> Contract
scaleK x = scale (konst x)

european :: Date -> Contract -> Contract
european t u = when (at t) (u `Contract.Contract.or` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) = anytime (between t1 t2)

zero :: Contract
zero = Zero
one :: Currency -> Contract
one = One
give :: Contract -> Contract
give = Give
and :: Contract -> Contract -> Contract
and = And
or :: Contract -> Contract -> Contract
or = Or
cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond
scale :: Obs Double -> Contract -> Contract
scale = Scale
when :: Obs Bool -> Contract -> Contract
when = When
anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime
until:: Obs Bool -> Contract -> Contract
until = Until

type RV a = [a]

newtype PR a = PR { unPr :: [RV a] } deriving Show

instance Functor PR where
    fmap f (PR a) = PR $ map (map f) a

instance Applicative PR where
    pure x = PR [[ x ]]
    (<*>) (PR f) (PR x) = PR $ zipWith (zipWith ($)) f x

newtype Obs a = Obs (Date -> PR a)

instance Functor Obs where
    fmap f (Obs dateToPRa) = Obs (\date -> PR (map (map f) (unPr $ dateToPRa date)))

instance Applicative Obs where
    pure x = Obs (\_ -> PR [[x]])
    (<*>) (Obs f) (Obs x) = Obs (\date -> f date <*> x date)

instance Show a => Show (Obs a) where
  show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

instance Num a => Num (Obs a) where
   fromInteger i = konst (fromInteger i)
   (+) = liftA2 (+)
   (-) = liftA2 (-)
   (*) = liftA2 (*)
   abs = fmap abs
   signum = fmap signum

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = liftA2 (<)
(%<=) = liftA2 (<=)
(%=)  = liftA2 (==)
(%>=) = liftA2 (>=)
(%>)  = liftA2 (>)

konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

date :: Obs Date

at :: Date -> Obs Bool
at t = date %= konst t

between :: Date -> Date -> Obs Bool
between t1 t2 = liftA2 (&&) (date %>= konst t1) (date %<= konst t2)

date = Obs (\t -> PR $ timeSlices [t])

time0 :: Date
time0 = mkDate 0

date :: String -> Date
date = undefined

bigK :: a -> PR a
bigK x = PR (konstSlices x)

timeSlices :: (Num t1,Enum t1) => [(t, t1)] -> [[(t, t1)]]
timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

konstSlices :: a -> [[a]]
konstSlices x = nextSlice [x]
  where nextSlice sl = sl : nextSlice (x:sl)

mkDate :: TimeStep -> Date
mkDate s = ((),s)