module Render
  ( renderObject
  , renderScene
  ) where

import Graphics.Rendering.OpenGL hiding (Face)
import Linear
import Linear.Affine
import Control.Lens hiding (transform)
import Data.Maybe

import Constraints.Scalar
import Object
import SceneTO
import Transformation
import Shaders
import Geometry.Polytope
import Geometry.Hyperplane
import Geometry.Combinatorics
import Constraints.Vector


renderObject :: (SomeScalar a) =>
  ShaderLocations -> Object V3 a -> IO ()
renderObject locs =
    mapM_ (uncurry (renderFace locs))
  . faceVertices (view facePlane)
  . view objectFaces

renderFace :: (SomeScalar a) =>
  ShaderLocations -> Face V3 a -> [Point V3 a] -> IO ()
renderFace locs f verts = do
  color (f ^. faceColor)
  normal' (f ^. facePlane . planeNormal)
  renderPrimitive Polygon $ mapM_ vertex' verts
  where
    vertex' (P (V3 x y z)) = vertex (Vertex3 x y z)
    normal' (V3 x y z) =
      vertexAttrib ToFloat (locs ^. aNormal) (Vector3 x y z)

renderScene :: (SomeVector v, R3 v, SomeScalar a) =>
  ShaderLocations -> SceneTO v a -> IO ()
renderScene locs =
    mapM_ (renderObject locs)
  . mapMaybe (intersectObject _xyz)
  . transformedSceneObjects
