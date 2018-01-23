{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Object
  ( Object(..)
  , objectCenter, objectRadius, objectInnerRadius, objectFaces
  , Face(..)
  , faceColor, facePlane
  , objectColor
  , simpleObject
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
  , _objectInnerRadius :: a
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

objectColor :: Traversal' (Object v a) Color
objectColor = objectFaces . each . faceColor

simpleObject :: (SomeVector v, Num a) => a -> [Hyperplane v a] -> Object v a
simpleObject radius faces = Object
  { _objectCenter = zero
  , _objectRadius = radius
  , _objectInnerRadius = 0
  , _objectFaces = map (Face defaultColor) faces
  }
  where
    defaultColor = grey

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
  else Just $ Object newCenter newRadius newInnerRadius newFaces
  where
    dist = norm $ (o ^. objectCenter . _Point) & lens .~ zero
    newCenter = (o ^. objectCenter . _Point . lens . from _Point)
    newRadius = reduceRadius (o ^. objectRadius)
    newInnerRadius = reduceRadius (o ^. objectInnerRadius)
    newFaces = (o ^. objectFaces)
      & each . facePlane . planeNormal %~ view lens
    reduceRadius r = let sq = r^2 - dist^2 in
      if sq <= 0 then 0 else sqrt sq
