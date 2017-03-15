module Render
  ( renderObject
  , renderScene
  ) where

import Graphics.Rendering.OpenGL hiding (Face)
import Control.Lens hiding (transform)
import Linear
import Linear.Affine

import Object
import SceneTO
import Transformation
import Shaders


renderObject :: (VertexComponent a, VertexAttribComponent a) =>
  ShaderLocations -> Object a -> IO ()
renderObject locs = mapM_ (renderFace locs) . view objectFaces

renderFace :: (VertexComponent a, VertexAttribComponent a) =>
  ShaderLocations -> Face a -> IO ()
renderFace locs f = do
  color (f ^. faceColor)
  normal' (f ^. faceNormal)
  renderPrimitive Polygon . mapM_ vertex' $ f ^. faceVertices
  where
    vertex' (P (V3 x y z)) = vertex (Vertex3 x y z)
    normal' (V3 x y z) =
      vertexAttrib ToFloat (locs ^. aNormal) (Vector3 x y z)

renderScene :: (VertexComponent a, VertexAttribComponent a, Num a) =>
  ShaderLocations -> SceneTO a -> IO ()
renderScene locs = mapM_ (renderObject locs . uncurry transform) . sceneObjects
