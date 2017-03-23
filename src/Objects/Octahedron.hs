module Objects.Octahedron
  ( octahedron
  ) where

import Linear
import Linear.Affine

import Object
import Color


octahedron :: (Floating a, Epsilon a) =>
  a -> Color -> Object a
octahedron radius c =
  Object zero radius . map (($ faceDist) . uncurry (Face c))
  $ octahedronFaces radius
  where
    faceDist = radius / sqrt 3

octahedronFaces :: (Floating a, Epsilon a) =>
  a -> [([Point V3 a], V3 a)]
octahedronFaces radius = do
  x <- [V3 (-1) 0 0, V3 1 0 0]
  y <- [V3 0 (-1) 0, V3 0 1 0]
  z <- [V3 0 0 (-1), V3 0 0 1]
  let faces = map (^* radius) [P x, P y, P z]
      normal = normalize (x ^+^ y ^+^ z)
  return (faces, normal)
