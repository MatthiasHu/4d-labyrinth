module Objects.Octahedron
  ( octahedron
  ) where

import Linear
import Linear.Affine

import Constraints.Scalar
import Object
import Color
import Geometry.Hyperplane


octahedron :: (SomeScalar a) =>
  a -> Color -> Object V3 a
octahedron radius c =
  Object zero radius 0 . map (Face c)
  $ octahedronFaces faceDist
  where
    faceDist = radius / sqrt 3

octahedronFaces :: (SomeScalar a) =>
  a -> [Hyperplane V3 a]
octahedronFaces faceDist = do
  x <- [-1, 1]
  y <- [-1, 1]
  z <- [-1, 1]
  return $ Hyperplane (V3 x y z) faceDist
