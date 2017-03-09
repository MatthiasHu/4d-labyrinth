module Objects.Tetrahedron
  ( tetrahedron
  ) where

import Linear
import Linear.Affine

import Object
import Color


tetrahedron :: (Floating a) => a -> Color -> Object a
tetrahedron radius c =
  Object . map (uncurry (Face c)) $ tetrahedronFaces radius

tetrahedronFaces :: Floating a => a -> [([Point V3 a], V3 a)]
tetrahedronFaces radius = do
  x <- [V3 (-1) 0 0, V3 1 0 0]
  y <- [V3 0 (-1) 0, V3 0 1 0]
  z <- [V3 0 0 (-1), V3 0 0 1]
  let faces = map (^* radius) [P x, P y, P z]
      normal = (x ^+^ y ^+^ z) / sqrt 3
  return (faces, normal)
