module RotationMethods
  ( RotationMethod
  , rot3d
  , rot4dPartial
  , rot4dQuaternion
  ) where

import Linear
import Data.Monoid

import Transformation


-- Methods of translating rotation input into rotations of the eye.
type RotationMethod v a = (a, a) -> Transformation v a


rot3d :: (Floating a) => RotationMethod V3 a
rot3d (dx, dy) =
     rotation _zx dx
  <> rotation _zy dy

rot4dPartial :: (Floating a) => RotationMethod V4 a
rot4dPartial (dx, dy) =
     rotation _zx dx
  <> rotation _zw dy

-- 4d "quaternion" rotations / isoclinic double rotations.
-- This allows only rotations of the eye that can be realized
-- by (left) multiplication with a quaternion.
-- (So the viewing direction completely determines the rotation.)
rot4dQuaternion :: (Floating a) => RotationMethod V4 a
rot4dQuaternion (dx, dy) =
     rotation _zx dx <> rotation _yw dx
  <> rotation _zy dy <> rotation _wx dy
