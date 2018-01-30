module Objects.Arrowhead
  ( arrowhead
  ) where

import Linear
import Control.Lens

import Object
import Color
import Geometry.Hyperplane


arrowhead :: (Floating a, Epsilon a) => Object V4 a
arrowhead =
  simpleObject radius [] & objectFaces .~
    Face grey (Hyperplane (zero & _z .~ 1) 0) :
    [ Face blue $
        Hyperplane (normalize $ zero & _xyw .~ v & _z .~ (-0.2)) 0.2
    | v <- cubeNormals ]
  where
    radius = 5  -- Wild guess.
    cubeNormals =
      [ V3   1   0   0
      , V3 (-1)  0   0
      , V3   0   1   0
      , V3   0 (-1)  0
      , V3   0   0   1
      , V3   0   0 (-1)
      ]
