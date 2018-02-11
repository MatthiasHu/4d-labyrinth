module Objects.Construction
  ( lineObject
  , prism, prism'
  , cone, cone'
  , addY, addZ, addW
  ) where


import Linear
import Linear.Affine
import Control.Lens hiding (prism, prism')

import Object
import Geometry.Hyperplane
import Color


lineObject :: (Fractional a) => a -> Object V1 a
lineObject length =
  simpleObject (length/2)
    [ Hyperplane (V1 (-1)) (length/2)
    , Hyperplane (V1   1 ) (length/2)
    ]

prism :: (Additive v, Floating a) =>
  (v a -> a -> v' a) -> a -> Color -> Color -> Object v a -> Object v' a
prism compose height colorTop colorBot o = Object
  { _objectCenter = P (compose (o ^. objectCenter . lensP) 0)
  , _objectRadius = sqrt ((o ^. objectRadius)^^2 + hh^^2)
  , _objectInnerRadius = 0
  , _objectFaces = map modifyFace (o ^. objectFaces)
      ++ [ Face colorTop $ Hyperplane (compose zero   1 ) hh
         , Face colorBot $ Hyperplane (compose zero (-1)) hh ]
  , _objectWireframe = Nothing
  }
  where
    hh = height/2
    modifyFace = facePlane . planeNormal %~ (flip compose 0)

prism' :: (Additive v, Floating a) =>
  (v a -> a -> v' a) -> a -> Object v a -> Object v' a
prism' compose height = prism compose height grey grey

cone :: (Metric v, Metric v', Floating a, Epsilon a) =>
  (v a -> a -> v' a) -> a -> Color -> Object v a -> Object v' a
cone compose height colorBot o = Object
  { _objectCenter = origin
  , _objectRadius = h-y
  , _objectInnerRadius = 0
  , _objectFaces = map (facePlane %~ modifyPlane) (o ^. objectFaces)
      ++ [ Face colorBot $ Hyperplane (compose zero (-1)) y ]
  , _objectWireframe = Nothing
  }
  where
    y = 0.5 * (h - (r^^2 / h))
    -- (h - y)^^2 == y^^2 + r^^2
    h = height
    r = norm (o ^. objectCenter) + (o ^. objectRadius)
    modifyPlane (Hyperplane n v) =
      mkHyperplane (compose n (v/h)) (v*(1-y/h))

cone' :: (Metric v, Metric v', Floating a, Epsilon a) =>
  (v a -> a -> v' a) -> a -> Object v a -> Object v' a
cone' compose height = cone compose height grey


-- Decompositions of vector spaces useful for prism and cone.

addY :: V1 a -> a -> V2 a
addY (V1 x) = V2 x

addZ :: V2 a -> a -> V3 a
addZ (V2 x y) = V3 x y

addW :: V3 a -> a -> V4 a
addW (V3 x y z) = V4 x y z
