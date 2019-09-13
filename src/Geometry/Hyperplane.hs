{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geometry.Hyperplane
  ( Hyperplane(..)
  , planeNormal, planeValue
  , planeDist
  , movePlane
  ) where

import Linear
import Linear.Affine
import Control.Lens hiding (transform)

import Constraints.Vector
import Transformation


-- A hyperplane (or half space) of a vector space.
-- The normal must be non-zero, but not necessarily of unit length.
-- (See planeDist below for the points lying on the hyperplane.)
data Hyperplane v a = Hyperplane
  { _planeNormal :: v a
  , _planeValue :: a
  }
  deriving (Show, Functor)

makeLenses ''Hyperplane

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
