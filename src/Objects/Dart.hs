{-# LANGUAGE ScopedTypeVariables #-}

module Objects.Dart
  ( dart
  ) where

import Control.Lens hiding (prism', transform)
import Linear

import SceneTO
import Object
import Objects.Cube
import Objects.Construction
import Transformation
import Color


dart :: forall a. (Floating a, Epsilon a) => SceneTO V4 a
dart = SceneFork . map (SceneObject . (\t -> transform t part)) $
  [ mempty :: Transformation V4 a
  , rotation _xy (tau/4)
  , rotation _xw (tau/4)
  ]
  where
    part = cone addW 0.6 grey cuboid
      & transform (rotation _zw (tau/4) :: Transformation V4 a)
    cuboid = lineObject 0.5
      & prism' addY 0.05
      & prism' addZ 0.05
      & objectColor .~ red
    tau = 2*pi
