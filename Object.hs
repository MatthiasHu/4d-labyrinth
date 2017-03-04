{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Object where

import Linear
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
-- with a color and a list of vertices
data Face a = Face
  { _faceColor :: Color
  , _faceVertices :: [V3 a]
  }
  deriving (Functor)

makeLenses ''Object
makeLenses ''Face

instance Transformable Face where
  transform t = over faceVertices $ map (transform t)

instance Transformable Object where
  transform t = over objectFaces $ map (transform t)
