{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..)
  , window
  , shaderLocs
  , scene
  , eye
  , lastTick
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import SDL
import Control.Lens
import Data.Word

import Shaders
import SceneTO
import Transformation


data State = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO V3 GLfloat
  , _eye :: Transformation V3 GLfloat
  , _lastTick :: Word32
  }

makeLenses ''State
