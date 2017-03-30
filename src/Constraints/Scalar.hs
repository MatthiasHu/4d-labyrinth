{-# LANGUAGE ConstraintKinds #-}

module Constraints.Scalar
  ( SomeScalar
  ) where

import Linear
import Linear.Affine
import Graphics.Rendering.OpenGL


-- Constraint for scalar type variables.
type SomeScalar a =
  ( Floating a
  , Ord a
  , Epsilon a
  , Show a
  , VertexComponent a
  , VertexAttribComponent a
  )
