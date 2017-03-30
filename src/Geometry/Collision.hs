module Geometry.Collision
  ( collisionInterval
  ) where

import Linear
import Linear.Affine
import Data.Foldable
import Control.Monad

import Geometry.Interval
import Geometry.Ray


type Inequality f a = (f a, a)


collisionInterval :: (Metric v, Foldable t, Ord a, Fractional a) =>
  Ray v a -> Interval a -> t (Inequality v a) -> Maybe (Interval a)
collisionInterval ray = foldM go
  where
    go oldInterval inequality = do
      newInterval <- fulfillingInterval ray inequality
      intersect newInterval oldInterval

fulfillingInterval :: (Metric v, Ord a, Fractional a) =>
  Ray v a -> Inequality v a -> Maybe (Interval a)
fulfillingInterval (P p, v) (n, a)
  | d >= 0     = upTo t0
  | otherwise  = fromOn t0
  where
    d = v `dot` n
    t0 = (a - (p `dot` n)) / d
