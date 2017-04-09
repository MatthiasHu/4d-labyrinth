{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Object
  ( Object(..)
  , objectCenter, objectRadius, objectFaces
  , Face(..)
  , faceColor, facePlane
  , inReach
  , intersectObject
  ) where

import Linear
import Linear.Affine
import Control.Lens hiding (transform)

import Constraints.Scalar
import Constraints.Vector
import Geometry.Hyperplane
import Color
import Transformation


-- A graphical object.
data Object v a = Object
  { _objectCenter :: Point v a
  , _objectRadius :: a
  , _objectFaces :: [Face v a]
  }
  deriving (Functor, Show)

-- A face of an object.
data Face v a = Face
  { _faceColor :: Color
  , _facePlane :: Hyperplane v a
  }
  deriving (Functor, Show)

makeLenses ''Object
makeLenses ''Face

instance (SomeVector v) => Transformable v (Face v) where
  transform = over facePlane . transform

instance (SomeVector v) => Transformable v (Object v) where
  transform t =
      (objectFaces . each %~ transform t)
    . (objectCenter %~ transform t)

inReach :: (SomeVector v, SomeScalar a) =>
  Point v a -> a -> Object v a -> Bool
inReach p r o =
  distance p (o ^. objectCenter) <= r + (o ^. objectRadius)

-- Intersect an object with a linear subspace.
intersectObject :: (Additive v', Metric v, Floating a, Ord a) =>
  Lens' (v a) (v' a) -> Object v a -> Maybe (Object v' a)
intersectObject lens o = if dist >= (o ^. objectRadius) then Nothing
  else Just $ Object newCenter newRadius newFaces
  where
    dist = norm $ (o ^. objectCenter . _Point) & lens .~ zero
    newCenter = (o ^. objectCenter . _Point . lens . from _Point)
    newRadius = sqrt $ (o ^. objectRadius)^2 - dist^2
    newFaces = (o ^. objectFaces)
      & each . facePlane . planeNormal %~ view lens
