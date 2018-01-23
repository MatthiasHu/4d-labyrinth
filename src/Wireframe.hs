{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Wireframe
  ( Wireframe(..)
  , wireframeLines
  , projectWireframe
  ) where

import Linear.Affine
import Control.Lens hiding (transform)

import Transformation
import Constraints.Vector


data Wireframe v a = Wireframe
  { _wireframeLines :: [(Point v a, Point v a)]
  }
  deriving (Eq, Ord, Show, Functor)

makeLenses ''Wireframe

instance (SomeVector v) => Transformable v (Wireframe v) where
  transform t = wireframeLines . each . both %~ transform t

projectWireframe :: Lens' (v a) (v' a) -> Wireframe v a -> Wireframe v' a
projectWireframe lens =
  wireframeLines . each . both %~ view (_Point . lens . from _Point)
