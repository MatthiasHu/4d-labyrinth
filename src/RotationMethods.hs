module RotationMethods
  ( RotationMethod
  , rot3d
  ) where

import Linear
import Data.Monoid

import Transformation


type RotationMethod v a = (a, a) -> Transformation v a


rot3d :: (Floating a) => RotationMethod V3 a
rot3d (dx, dy) =
     rotation _zx dx
  <> rotation _zy dy
