{-# LANGUAGE TemplateHaskell #-}

module State
  ( Scalar
  , State(..)
  , window
  , shaderLocs
  , scene
  , blockworld
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
import Blockworld
import Transformation
import RotationMethods


type Scalar = GLfloat

data State v = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO v Scalar
  , _blockworld :: Blockworld v
  , _eye :: Transformation v Scalar
  , _rotationMethod :: RotationMethod v Scalar
  , _lastTick :: Word32
  }

makeLenses ''State
