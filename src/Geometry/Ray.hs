module Geometry.Ray
  ( Ray
  ) where

import Linear.Affine


type Ray v a = (Point v a, v a)
