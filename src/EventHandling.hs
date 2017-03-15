{-# LANGUAGE TemplateHaskell #-}

module EventHandling
  ( State(..)
  , window, shaderLocs, scene, eye, lastTick
  , handleEvent
  ) where

import SDL
import Graphics.Rendering.OpenGL (GLfloat)
import Control.Lens
import Data.Int (Int32)
import Data.Monoid
import Data.Word

import SceneTO
import Transformation
import Setup
import Shaders


data State = State
  { _window :: Window
  , _shaderLocs :: ShaderLocations
  , _scene :: SceneTO GLfloat
  , _eye :: Transformation GLfloat
  , _lastTick :: Word32
  }

makeLenses ''State


handleEvent :: EventPayload -> State -> IO State
handleEvent (WindowClosedEvent _) = const quitAndExit
handleEvent (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) =
  handleKeyPressed keysym
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ _ d)) =
  handleMouseMotion d
handleEvent (WindowSizeChangedEvent _) =
  handleWindowSizeChanged
handleEvent _ = return

handleKeyPressed :: Keysym -> State -> IO State
handleKeyPressed (Keysym _ KeycodeEscape _) _ = quitAndExit
handleKeyPressed _ s = return s

handleMouseMotion :: V2 Int32 -> State -> IO State
handleMouseMotion (V2 dx dy) = return . over eye
  ((   rotation _xz (fromIntegral (-dx) * sensitivity)
    <> rotation _yz (fromIntegral dy    * sensitivity) ) <>)
  where
    sensitivity = 0.005

handleWindowSizeChanged :: State -> IO State
handleWindowSizeChanged s = do
  setViewport (s ^. window)
  return s
