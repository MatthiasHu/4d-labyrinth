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
  a -> Color -> Object v a
cube radius c = Object zero (sqrt dim * radius) $
  map (Face c) (cubeFaces radius)
  where
    dim = sum (pure 1 :: v a)

cubeFaces :: (SomeVector v, Floating a) => a -> [Hyperplane v a]
cubeFaces radius = do
  v <- basis
  sign <- [-1, 1]
  return $ Hyperplane (sign *^ v) radius
