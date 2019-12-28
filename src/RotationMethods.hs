{-# LANGUAGE TemplateHaskell #-}

module RotationMethods
  ( RotationInput
  , RotationMethod(..)
  , inputRotation
  , inactiveRotation
  , rot3d
  , rot4dPartial
  , rot4dQuaternion
  ) where

import Linear
import Control.Lens
import Data.Monoid

import Transformation


type RotationInput a = (a, a, a)

-- Methods of translating rotation input into rotations of the eye.
data RotationMethod v a = RotationMethod
  { _inputRotation :: RotationInput a -> Transformation v a
  , _inactiveRotation :: a -> Transformation v a
  }

makeLenses ''RotationMethod


rot3d :: (Floating a) => RotationMethod V3 a
rot3d = RotationMethod
  (\(dx, dy, dz) ->
        rotation _zx dx
     <> rotation _zy dy
     <> rotation _xy dz
  )
  (const mempty)


rot4dPartial :: (Floating a) => RotationMethod V4 a
rot4dPartial = RotationMethod
  (\(dx, dy, dz) -> rotation _zx dx <> rotation _zw dy)
  (const mempty)

-- 4d "quaternion" rotations / isoclinic double rotations.
-- This allows only rotations of the eye that can be realized
-- by (left) multiplication with a (unit) quaternion.
-- (So the viewing direction completely determines the rotation.)
rot4dQuaternion :: (Floating a) => RotationMethod V4 a
rot4dQuaternion = RotationMethod
  (\(dx, dy, dz) ->
        rotation _zx dx <> rotation _yw dx
     <> rotation _zy dy <> rotation _wx dy
     <> rotation _zw dz <> rotation _xy dz
  )
  (\d -> rotation _zw d <> rotation _xy d)
