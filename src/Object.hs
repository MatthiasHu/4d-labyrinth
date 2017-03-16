{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Object
  ( Object(..)
  , objectFaces
  , Face(..)
  , faceColor, faceVertices, faceNormal, faceDistance
  , boundingInequalities
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
  , _faceDistance :: a
  }
  deriving (Functor)

makeLenses ''Object
makeLenses ''Face

instance Transformable Face where
  transform t f = f &
      (faceVertices . each %~ transform t)
    . (faceNormal %~ transform t)
    . (faceDistance +~ translationPart t `dot` (f ^. faceNormal))

instance Transformable Object where
  transform t = objectFaces . each %~ transform t

boundingInequalities :: Object a -> [(V3 a, a)]
boundingInequalities =
  map (\f -> (f ^. faceNormal, f ^. faceDistance)) . view objectFaces
