{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Object
  ( Object(..)
  , objectCenter, objectRadius, objectFaces
  , Face(..)
  , faceColor, facePlane
  , inReach
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
