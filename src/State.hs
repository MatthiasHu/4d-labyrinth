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
  , inactiveFor
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import SDL
import Control.Lens
import Data.Word

import Shaders
import SceneTO
import Transformation
import RotationMethods
import Constraints.Vector


type Scalar = GLfloat

data State v = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO v Scalar
  , _eye :: Transformation v Scalar
  , _rotationMethod :: RotationMethod v Scalar
  , _lastTick :: Word32
  , _inactiveFor :: Int
  }

makeLenses ''State
