module Worlds.Tessellation
  ( Tessellation(..)
  , cubes
  , diamonds
  ) where

import Linear hiding (translation)

import Constraints.Vector
import Constraints.Scalar
import Object
import SceneTO
import Transformation
import Objects.Cube
import Objects.Diamond
import Geometry.Combinatorics


data Tessellation p v = Tessellation
  { neighbours :: p -> [p]
  , point :: p -> v Float
  , tile :: p -> SceneTO v Float
  , origin :: p
  }
-- TODO: Make more polymorphic (instead of Float)?

cubes :: (SomeVector v) =>
  Tessellation (v Int) v
cubes = Tessellation neighbours point tile origin
  where
    neighbours p = [ p ^+^ sign *^ e | e <- basis, sign <- [-1, 1] ]
    point = fmap fromIntegral
    tile p =
      Transformed (translation $ point p) (SceneObject $ cube 0.5)
    origin = pure 0

diamonds :: (SomeVector v) =>
  Tessellation (v Int) v
diamonds = Tessellation neighbours point tile origin
  where
    neighbours p =
      [ p ^+^ s1*^v1 ^+^ s2*^v2
      | [v1, v2] <- choose' 2 basis
      , s1 <- [-1, 1]
      , s2 <- [-1, 1]
      ]
    point = fmap fromIntegral
    tile p =
      Transformed (translation $ point p)
        (SceneObject $ diamond 0.5)
    origin = pure 0
