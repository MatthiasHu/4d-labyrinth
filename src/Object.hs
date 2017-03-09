{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Object
  ( Object(..)
  , objectFaces
  , Face(..)
  , faceColor, faceVertices, faceNormal
  ) where

import Linear
import Linear.Affine
import Control.Lens hiding (transform)

import Color
import Transformation


-- a graphical object,
-- consisting of some faces
data Object a = Object
  { _objectFaces :: [Face a]
  }
  deriving (Functor)

-- a face of an object,
-- with a color, a list of vertices
-- and a plane normal
data Face a = Face
  { _faceColor :: Color
  , _faceVertices :: [Point V3 a]
  , _faceNormal :: V3 a
  }
  deriving (Functor)

makeLenses ''Object
makeLenses ''Face

instance Transformable Face where
  transform t =
      (over faceVertices $ map (transform t))
    . (over faceNormal $ transform t)

instance Transformable Object where
  transform t = over objectFaces $ map (transform t)
