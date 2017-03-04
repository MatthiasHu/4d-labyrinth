module Render
  ( renderObject
  ) where

import Graphics.Rendering.OpenGL hiding (Face)
import Control.Lens
import Linear

import Object


renderObject :: (VertexComponent a) => Object a -> IO ()
renderObject = mapM_ renderFace . view objectFaces

renderFace :: (VertexComponent a) => Face a -> IO ()
renderFace (Face c vertices) = do
  color c
  renderPrimitive Polygon $ mapM_ vertex' vertices
  where
    vertex' (V3 x y z) = vertex (Vertex3 x y z)
