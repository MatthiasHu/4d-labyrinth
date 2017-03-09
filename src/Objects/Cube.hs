module Objects.Cube
  ( cube
  ) where

import Linear
import Linear.Affine
import Control.Lens

import Object
import Color


cube :: (Floating a) => Color -> Object a
cube c = Object $ map (uncurry (Face c)) cubeFaces

cubeFaces :: (Floating a) => [([Point V3 a], V3 a)]
cubeFaces = do
  (a, b, c) <- [(_x, _y, _z), (_y, _z, _x), (_z, _x, _y)]
  sign <- [-1, 1]
  let p = pure sign
      normal = zero & c .~ sign
      vertices = map (^* radius) $
        [ p
        , p & a %~ negate
        , p & a %~ negate & b %~ negate
        , p & b %~ negate
        ]
  return (vertices, normal)
  where radius = 0.5
