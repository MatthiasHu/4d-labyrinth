module Geometry.Collision
  ( collisionInterval
  ) where

import Linear
import Linear.Affine
import Control.Lens
import Control.Monad
import Data.Foldable

import Geometry.Interval
import Geometry.Ray
import Geometry.Hyperplane


-- Interval of collision of a ray with a polytope given by Hyperplanes.
collisionInterval :: (Metric v, Foldable t, Ord a, Fractional a) =>
  Ray v a -> Interval a -> t (Hyperplane v a) -> Maybe (Interval a)
collisionInterval ray = foldM go
  where
    go oldInterval plane = do
      newInterval <- behindInterval ray plane
      intersect newInterval oldInterval

behindInterval :: (Metric v, Ord a, Fractional a) =>
  Ray v a -> Hyperplane v a -> Maybe (Interval a)
behindInterval (P p, v) plane
  | d >= 0     = upTo t0
  | otherwise  = fromOn t0
  where
    n = plane ^. planeNormal
    a = plane ^. planeValue
    d = v `dot` n
    t0 = (a - (p `dot` n)) / d
