{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Setup
  ( setup
  , quitAndExit
  , setViewport
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
  setupGL window
  return window

windowConfig = defaultWindow
  { windowOpenGL = Just defaultOpenGL
  , windowMode = FullscreenDesktop
  }

setupGL :: Window -> IO ()
setupGL w = do
  -- setup viewport and projection matrix
  setViewport w
  -- set background color
  clearColor $= Color4 0 0 0 1
  -- depth test
  depthFunc $= Just Lequal

quitAndExit :: IO a
quitAndExit = quit >> exitSuccess

setViewport :: Window -> IO ()
setViewport w = do
  V2 x y <- glGetDrawableSize w
  viewport $= (Position 0 0, Size (fI x) (fI y))
  setProjectionMatrix ((fI x) / (fI y))
  where
    fI :: (Integral a, Num b) => a -> b
    fI = fromIntegral

setProjectionMatrix :: GLdouble -> IO ()
setProjectionMatrix aspect = do
  matrixMode $= Projection
  loadIdentity
  let near = 10**(-3)
      far = 10**3
      x = near * aov * sqrt aspect
      y = near * aov / sqrt aspect
  GL.frustum (-x) x (-y) y near far
  matrixMode $= Modelview 0
  where
    -- angle of view ratio
    aov = 1
