{-# LANGUAGE OverloadedStrings #-}

module Setup
  ( setup
  , setViewport
  , quitAndExit
  ) where

import SDL
import Graphics.Rendering.OpenGL as GL
import System.Exit

import Shaders


setup :: IO (Window, ShaderLocations)
setup = do
  initialize [InitVideo]
  _ <- setMouseLocationMode RelativeLocation
  window <- createWindow "3d labyrinth" windowConfig
  shaderLocs <- setupGL window
  return (window, shaderLocs)

windowConfig :: WindowConfig
windowConfig = defaultWindow
  { windowOpenGL = Just defaultOpenGL
  , windowMode = FullscreenDesktop
  }

setupGL :: Window -> IO ShaderLocations
setupGL w = do
  _ <- glCreateContext w
  shaderLocs <- setupShaders
  setViewport w
  clearColor $= Color4 0 0 0 1
  return shaderLocs

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

quitAndExit :: IO a
quitAndExit = quit >> exitSuccess
