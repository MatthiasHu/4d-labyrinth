{-# LANGUAGE ConstraintKinds #-}

module Constraints.Vector
  ( SomeVector
  ) where

import Linear
import Data.Functor.Rep


-- Constraint for vector type variables.
type SomeVector v =
  ( Applicative v
  , Traversable v
  , Metric v
  , Representable v
  , Ord (v Int)
  , R3 v
  )
