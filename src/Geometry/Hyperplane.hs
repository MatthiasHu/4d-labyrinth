{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geometry.Hyperplane
  ( Hyperplane(..)
  , planeNormal, planeValue
  , mkHyperplane
  , planeDist
  , movePlane
  ) where

import Linear
import Linear.Affine
import Control.Lens hiding (transform)

import Constraints.Vector
import Transformation


-- A hyperplane (or half space) of a vector space.
data Hyperplane v a = Hyperplane
  { _planeNormal :: v a
  , _planeValue :: a
  }
  deriving (Show, Functor)

makeLenses ''Hyperplane

mkHyperplane :: (Metric v, Floating a, Epsilon a) =>
  v a -> a -> Hyperplane v a
mkHyperplane n0 v0 = Hyperplane (n0 ^/ s) (v0 / s)
  where
    s = norm n0

-- Positive values mean 'in front of the plane'
-- (or 'outside the half space'),
-- negative values mean 'behind the plane'
-- (or 'in the half space').
planeDist :: (Metric v, Num a) => Hyperplane v a -> Point v a -> a
planeDist (Hyperplane n v) (P p) = n `dot` p - v

instance (SomeVector v) => Transformable v (Hyperplane v) where
  transform t (Hyperplane n v) =
    let n' = transform t n
        v' = v + (translationPart t `dot` n')
    in Hyperplane n' v'

movePlane :: (Num a) => a -> Hyperplane v a -> Hyperplane v a
movePlane d = planeValue %~ (+d)
