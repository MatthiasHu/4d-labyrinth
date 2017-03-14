module Collision
  ( collisionInterval
  ) where

import Linear
import Linear.Affine
import Data.Foldable
import Data.Ord (comparing)
import Control.Monad

import Interval


type Ray f a = (Point f a, f a)

type Inequality f a = (f a, a)


collisionInterval :: (Metric f, Foldable t, Ord a, Fractional a) =>
  Ray f a -> t (Inequality f a) -> Maybe (Interval a)
collisionInterval ray = foldM go wholeLine
  where
    go oldInterval inequality = do
      newInterval <- fulfillingInterval ray inequality
      intersect newInterval oldInterval

fulfillingInterval :: (Metric f, Ord a, Fractional a) =>
  Ray f a -> Inequality f a -> Maybe (Interval a)
fulfillingInterval (P p, v) (n, a)
  | d >= 0     = upTo t0
  | otherwise  = fromOn t0
  where
    d = v `dot` n
    t0 = (a - (p `dot` n)) / d
