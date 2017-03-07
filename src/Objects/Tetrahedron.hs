module Objects.Tetrahedron
  ( tetrahedron
  ) where

import Linear

import Object
import Color


tetrahedron :: (Floating a) => a -> Color -> Object a
tetrahedron radius c = Object $ map (Face c) faces
  where
    faces = do
      x <- [-1, 1]
      y <- [-1, 1]
      z <- [-1, 1]
      return . map (^* radius) $
        [V3 x 0 0, V3 0 y 0, V3 0 0 z]
