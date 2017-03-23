{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Functor.Rep
import Data.Distributive


-- Parametrized matrix type.
type M v a = v (v a)

-- A roto-translation / rigid transformation of euclidean space,
-- that is, an orthogonal, affine mapping, preserving orientation.
data Transformation v a = Transformation
  { rotationPart :: M v a
  , translationPart :: v a
  }

deriving instance (Show (v a), Show (M v a)) =>
  Show (Transformation v a)
deriving instance (Eq (v a), Eq (M v a)) =>
  Eq (Transformation v a)

class Transformable v f where
  transform :: (Num a) => Transformation v a -> f a -> f a

-- Apply the trnsformation to a point.
instance (Additive v, Foldable v) =>
  Transformable v (Point v) where
  transform (Transformation rot trans) (P p) = (P (rot !* p) .+^ trans)

-- Apply the (rotation part of the) transformation to a vector.
-- This is appropriate when the vector does not represent
-- a point in space, but rather a direction (with length),
-- like normal vectors do.
instance (Foldable v, Additive v) =>
  Transformable v v where
  transform (Transformation rot _) v = rot !* v

-- A pure translation.
translation :: (Applicative v, Traversable v, Num a) =>
  v a -> Transformation v a
translation t = Transformation identity t

-- A pure rotation.
-- The lens specifies the plane to be rotated,
-- the angle is in radians.
rotation :: forall v a.
  ( Traversable v, Applicative v, Representable v, Additive v
  , Floating a
  ) =>
  (forall b. (Floating b) => Lens' (v b) (V2 b))
  -> a -> Transformation v a
rotation plane angle = Transformation
  { rotationPart = identity & set l
      (V2 (V2 c (-s)) (V2 s c))
  , translationPart = zero
  }
  where
    c = cos angle
    s = sin angle
    l :: Lens' (M v a) (M22 a)
    l = column plane . plane

-- Composition and identity transformation.
instance (Applicative v, Traversable v, Additive v, Num a) =>
  Monoid (Transformation v a) where
  mempty = Transformation identity zero
  mappend (Transformation rot2 trans2) (Transformation rot1 trans1) =
    Transformation (rot2 !*! rot1) ((rot2 !* trans1) ^+^ trans2)

-- Actually, rigid transformations form a group, not just a monoid.
-- (And computing the inverse is not very expensive,
-- because the rotation matrix is orthogonal.)
invert :: (Additive v, Foldable v, Distributive v, Num a) =>
  Transformation v a -> Transformation v a
invert (Transformation rot trans) =
  Transformation rot' (negated (rot' !* trans))
  where rot' = transpose rot

-- Note:
-- 'transform' is a group action:
--   transform (s <> t) == transform s . transform t
