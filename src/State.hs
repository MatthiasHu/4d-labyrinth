{-# LANGUAGE TemplateHaskell #-}

module State
  ( Scalar
  , State(..)
  , window
  , shaderLocs
  , scene
  , eye
  , rotationMethod
  , lastTick
  , placeableObject
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import SDL
import Control.Lens
import Data.Word

import Shaders
import SceneTO
import Transformation
import RotationMethods
import Object
import Constraints.Vector


type Scalar = GLfloat

data State v = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO v Scalar
  , _eye :: Transformation v Scalar
  , _rotationMethod :: RotationMethod v Scalar
  , _lastTick :: Word32
  , _placeableObject :: Maybe (Object v Scalar)
  }

makeLenses ''State
