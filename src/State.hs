{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..)
  , window
  , shaderLocs
  , scene
  , eye
  , rotationMethod
  , lastTick
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


data State v = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO v GLfloat
  , _eye :: Transformation v GLfloat
  , _rotationMethod :: RotationMethod v GLfloat
  , _lastTick :: Word32
  }

makeLenses ''State
