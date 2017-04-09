{-# LANGUAGE ScopedTypeVariables #-}

module Objects.Cube
  ( cube
  ) where

import Linear
import Linear.Affine
import Control.Lens

import Object
import Color
import Geometry.Hyperplane
import Constraints.Vector


cube :: forall v a. (SomeVector v, Floating a) =>
  Color -> Object v a
cube c = Object zero (sqrt dim * radius) $
  map (Face c) cubeFaces
  where
    dim = sum (pure 1 :: v a)

cubeFaces :: (SomeVector v, Floating a) => [Hyperplane v a]
cubeFaces = do
  v <- basis
  sign <- [-1, 1]
  return $ Hyperplane (sign *^ v) radius

radius :: (Fractional a) => a
radius = 0.5
