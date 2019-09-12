module Worlds.RandomScene
  ( randomScene
  , randomScene2
  ) where

import SceneTO
import Transformation
import Color
import Objects.Cube
import Objects.Diamond
import Object

import Linear hiding (translation)
import Control.Lens


randomScene :: (Monad m, Floating a) =>
  m (SceneTO V3 a, Transformation V3 a)
randomScene = return (scene, eye)
  where
    scene = Transformed (translation $ V3 1 0 (-4))
      $ SceneObject (cube 1.0 & objectColor .~ red)
    eye = mempty

randomScene2 :: (Monad m, Floating a, Ord a) =>
  Int -> m (SceneTO V4 a, Transformation V4 a)
randomScene2 n = return (scene, eye)
  where
    scene = SceneFork
      . map (\(t, o) -> Transformed (translation t) $ SceneObject o) $
      [ (t ^+^ V4 0 0 (-3 * fromIntegral z) 0, o)
      | z <- [0..n], (t, o) <-
      [ (V4 0 0 (-4) 0, diamond 1.0 & objectColor .~ red)
      , (V4 4 0 (-4) 0, diamond 1.0 & objectColor .~ blue)
      , (V4 2 2 (-4) 0, diamond 1.0 & objectColor .~ green)
      , (V4 2 0 (-4) 2, diamond 1.0 & objectColor .~ red)
      ]
      ]
    eye = mempty
