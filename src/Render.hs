module Render
  ( renderObject
  , renderScene
  ) where

import Graphics.Rendering.OpenGL hiding (Face)
import Control.Lens hiding (transform)
import Linear

import Object
import Scene
import Transformation


renderObject :: (VertexComponent a) => Object a -> IO ()
renderObject = mapM_ renderFace . view objectFaces

renderFace :: (VertexComponent a) => Face a -> IO ()
renderFace (Face c vertices) = do
  color c
  renderPrimitive Polygon $ mapM_ vertex' vertices
  where
    vertex' (V3 x y z) = vertex (Vertex3 x y z)

renderScene :: (VertexComponent a, Num a) =>
  Scene (Transformation a) (Object a) -> IO ()
renderScene = mapM_ (renderObject . uncurry transform) . sceneObjects
