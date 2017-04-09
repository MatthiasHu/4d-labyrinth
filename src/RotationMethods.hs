module RotationMethods
  ( RotationMethod
  , rot3d
  , rot4dpartial
  ) where

import Linear
import Data.Monoid

import Transformation


type RotationMethod v a = (a, a) -> Transformation v a


rot3d :: (Floating a) => RotationMethod V3 a
rot3d (dx, dy) =
     rotation _zx dx
  <> rotation _zy dy


rot4dpartial :: (Floating a) => RotationMethod V4 a
rot4dpartial (dx, dy) =
     rotation _zx dx
  <> rotation _zw dy
