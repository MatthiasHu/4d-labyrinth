{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Transformation
  ( Transformation
  , rotationPart, translationPart
  , Transformable
  , transform
  , translation
  , rotation
  , invert
  ) where

import Linear hiding (translation)
import Linear.Affine
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

-- Apply the trnsformation to a point.
instance Transformable (Point V3) where
  transform (Transformation rot trans) (P p) = (P (rot !* p) .+^ trans)

-- Apply the (rotation part of the) transformation to a vector.
-- This is appropriate when the vector does not represent
-- a point in space, but rather a direction (with length),
-- like normal vectors do.
instance Transformable V3 where
  transform (Transformation rot _) v = rot !* v

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

-- Composition and identity transformation.
instance (Num a) => Monoid (Transformation a) where
  mempty = Transformation identity zero
  mappend (Transformation rot2 trans2) (Transformation rot1 trans1) =
    Transformation (rot2 !*! rot1) ((rot2 !* trans1) + trans2)

-- Actually, rigid transformations form a group, not just a monoid.
-- (And computing the inverse is not very expensive,
-- because the rotation matrix is orthogonal.)
invert :: (Num a) => Transformation a -> Transformation a
invert (Transformation rot trans) =
  Transformation rot' (negated (rot' !* trans))
  where rot' = transpose rot

-- Note:
-- 'transform' is a group action:
--   transform (s <> t) == transform s . transform t
