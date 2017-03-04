{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformation
  ( Transformation
  , Transformable
  , transform
  , rotate
  , translation
  , rotation
  ) where

import Linear hiding (transform, rotate, translation)
import Control.Lens hiding (transform)


-- A roto-translation / rigid transformation of 3d space, that is,
-- an orthogonal, affine mapping, preserving orientation.
data Transformation a = Transformation
  { rotationPart :: M33 a
  , translationPart :: V3 a
  }
  deriving (Show, Eq)

-- Note:
-- I am neither using 4x4 roto-translation matrices
-- nor quaternions here,
-- because this is a warmup exercise for 4d stuff.

class Transformable f where
  transform :: (Num a) => Transformation a -> f a -> f a

-- Apply the transformation to a vector.
instance Transformable V3 where
  transform (Transformation rot trans) v = rot !* v + trans

-- Only apply the rotation part to a vector.
-- This is appropriate when the vector does not represent a point in space
-- but rather a direction (with length), like normal vectors.
rotate :: (Num a) => Transformation a -> V3 a -> V3 a
rotate (Transformation rot _) v = rot !* v

-- A pure translation.
translation :: (Num a) => V3 a -> Transformation a
translation t = Transformation identity t

-- A pure rotation.
-- The lens specifies the plane to be rotated,
-- the angle is in radians.
rotation :: forall a. (Floating a) =>
  (forall b. (Floating b) => Lens' (V3 b) (V2 b))
  -> a -> Transformation a
rotation plane angle = Transformation
  { rotationPart = identity & set l
      (V2 (V2 c (-s)) (V2 s c))
  , translationPart = zero
  }
  where
    c = cos angle
    s = sin angle
    l :: Lens' (M33 a) (M22 a)
    l = column plane . plane
