module Objects.Cube
  ( cube
  ) where

import Linear
import Control.Lens

import Object
import Color


cube :: (Floating a) => Color -> Object a
cube c = Object $ map (Face c) cubeFaces

cubeFaces :: (Floating a) => [[V3 a]]
cubeFaces = do
  (a, b) <- [(_x, _y), (_y, _z), (_z, _x)]
  sign <- [-1, 1]
  let p = pure sign
  return . map (^* radius) $
    [ p
    , p & a %~ negate
    , p & a %~ negate & b %~ negate
    , p & b %~ negate
    ]
  where radius = 0.5
