module Objects.Cube
  ( cube
  ) where

import Linear
import Linear.Affine
import Control.Lens

import Object
import Color
import Geometry.Hyperplane


cube :: (Floating a) => Color -> Object V3 a
cube c = Object zero (sqrt 3 * radius) $
  map (Face c) cubeFaces

cubeFaces :: (Floating a) => [Hyperplane V3 a]
cubeFaces = do
  a <- [_x, _y, _z]
  sign <- [-1, 1]
  return $ Hyperplane (zero & a .~ sign) radius

radius :: (Fractional a) => a
radius = 0.5
