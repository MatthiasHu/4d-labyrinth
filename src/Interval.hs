module Interval
  ( Interval
  , lowerBound, upperBound
  , mkInterval
  , wholeLine
  , upTo, fromOn
  , intersect
  ) where


-- Open intervals of real numbers.

data Interval a = Interval
  { lowerBound :: a
  , upperBound :: a
  }
  deriving (Show)

-- "Maybe (Interval a)" is understood as
-- "an interval or the empty set".

mkInterval :: (Ord a) => a -> a -> Maybe (Interval a)
mkInterval l u | l < u      = Just (Interval l u)
               | otherwise  = Nothing

negInfty, posInfty :: (Fractional a) => a
negInfty = -1/0
posInfty = 1/0

wholeLine :: (Fractional a, Ord a) => Interval a
wholeLine = interval
  where
    Just interval = mkInterval negInfty posInfty

upTo, fromOn :: (Ord a, Fractional a) => a -> Maybe (Interval a)
upTo u   = mkInterval negInfty u
fromOn l = mkInterval l posInfty

intersect :: (Ord a) => Interval a -> Interval a -> Maybe (Interval a)
intersect (Interval l1 u1) (Interval l2 u2)
  | u1 <= l2 || u2 <= l1  = Nothing
  | otherwise  = Just $ Interval (max l1 l2) (min u1 u2)
