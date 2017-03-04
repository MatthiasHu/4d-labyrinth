{-# LANGUAGE OverloadedStrings #-}

module Setup
  ( setup
  , quitAndExit
  ) where

import SDL
import Graphics.Rendering.OpenGL as GL
import System.Exit


setup :: IO Window
setup = do
  initialize [InitVideo]
  setMouseLocationMode RelativeLocation
  window <- createWindow "3d labyrinth" windowConfig
  glContext <- glCreateContext window
  setupGL
  return window

windowConfig = defaultWindow
  { windowOpenGL = Just defaultOpenGL
  , windowInitialSize = V2 800 800
  }

setupGL :: IO ()
setupGL = do
  -- set background color
  clearColor $= Color4 0 0 0 1
  -- setup projection matrix
  matrixMode $= Projection
  loadIdentity
  GL.frustum (-0.1) 0.1 (-0.1) 0.1 0.1 100
  matrixMode $= Modelview 0
  -- depth test
  depthFunc $= Just Lequal

quitAndExit :: IO a
quitAndExit = quit >> exitSuccess
