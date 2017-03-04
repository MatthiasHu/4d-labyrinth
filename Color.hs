module Color
  ( Color
  , red
  ) where

import Graphics.Rendering.OpenGL hiding (Color)

type Color = Color3 GLfloat


red :: Color
red = Color3 1 0 0
