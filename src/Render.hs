module Render
  ( renderScene
  ) where

import Graphics.Rendering.OpenGL hiding (Face)
import qualified Data.Vector.Storable as VS
import Linear hiding (translation)
import Linear.Affine
import Control.Lens hiding (transform)
import Data.Maybe

import Constraints.Scalar
import Object
import SceneTO
import Wireframe
import Shaders
import Geometry.Polytope
import Geometry.Hyperplane
import Color
import Transformation
import Constraints.Vector


-- render object, ray-tracing intersection of half spaces on the gpu
renderObject' :: (SomeScalar a) =>
  ShaderLocations -> Object V3 a -> IO ()
renderObject' locs obj = do
  let
    ofFaces lens = VS.fromList (obj ^.. objectFaces . each . lens)
    normals = ofFaces (facePlane . planeNormal . to v3ToVec3)
    values  = ofFaces (facePlane . planeValue)
    colors  = ofFaces faceColor
  uniformv' (locs ^. uHyperplaneNormals) normals
  uniformv' (locs ^. uHyperplaneValues) values
  uniformv' (locs ^. uHyperplaneColors) colors
  -- cover rear faces of a bounding box
  renderPrimitive Quads . mapM_ vertex' . concat $
    [ quad
    | (quad, h) <- boundingBoxQuads obj
    , planeDist h origin <= 0
    ]

boundingBoxQuads :: (SomeScalar a) =>
  Object V3 a -> [([Point V3 a], Hyperplane V3 a)]
boundingBoxQuads obj =
  [ ( [ adjust (a*^x ^+^ b*^y ^+^ z)
      | (a, b) <- [(1, 1), (1, -1), (-1, -1), (-1, 1)]
      ]
    , transform (translation (obj ^. objectCenter . _Point))
        (Hyperplane z (obj ^. objectRadius))
    )
  | (x, y, z) <-
    [ (V3 1 0 0, V3 0 1 0, V3 0 0 1)
    , (V3 0 1 0, V3 0 0 1, V3 1 0 0)
    , (V3 0 0 1, V3 1 0 0, V3 0 1 0)
    , (V3 (-1) 0 0, V3 0 (-1) 0, V3 0 0 (-1))
    , (V3 0 (-1) 0, V3 0 0 (-1), V3 (-1) 0 0)
    , (V3 0 0 (-1), V3 (-1) 0 0, V3 0 (-1) 0)
    ]
  ]
  where
    adjust = (obj ^. objectCenter .+^) . (^* obj ^. objectRadius)

uniformv' :: (VS.Storable a, Uniform a) =>
  UniformLocation -> VS.Vector a -> IO ()
uniformv' ul v = VS.unsafeWith v $ \ptr ->
  uniformv ul (fromIntegral $ VS.length v) ptr

-- old-style render object, computing vertices on the cpu
renderObject :: (SomeScalar a) =>
  ShaderLocations -> Object V3 a -> IO ()
renderObject locs obj = do
  uniform (locs ^. uObjectCenter) $=
    (v3ToVec3 $ obj ^. objectCenter . lensP)
  uniform (locs ^. uObjectInnerRadius) $=
    (obj ^. objectInnerRadius)
  mapM_ (uncurry (renderFace locs))
    (faceVertices (view facePlane) . view objectFaces $ obj)

renderFace :: (SomeScalar a) =>
  ShaderLocations -> Face V3 a -> [Point V3 a] -> IO ()
renderFace locs f verts = do
  color (f ^. faceColor)
  normal' (f ^. facePlane . planeNormal)
  renderPrimitive Polygon $ mapM_ vertex' verts
  where
    normal' n =
      vertexAttrib ToFloat (locs ^. aNormal) (v3ToVec3 n)

renderWireframe :: (SomeScalar a) =>
  Wireframe V3 a -> IO ()
renderWireframe wf = do
  color white
  mapM_ renderLine (wf ^. wireframeLines)
  where
    renderLine (a, b) = renderPrimitive Lines $ vertex' a >> vertex' b

vertex' :: (VertexComponent a) => Point V3 a -> IO ()
vertex' (P (V3 x y z)) = vertex (Vertex3 x y z)

v3ToVec3 :: V3 a -> Vector3 a
v3ToVec3 (V3 x y z) = Vector3 x y z

renderScene :: (SomeVector v, R3 v, SomeScalar a) =>
  ShaderLocations -> SceneTO v a -> IO ()
renderScene locs scene = do
  let os = transformedSceneObjects scene
  prepareRenderObjects' locs
  mapM_ (renderObject' locs) . mapMaybe (intersectObject _xyz) $ os
  prepareRenderWireframes
  mapM_ (renderWireframe . projectWireframe _xyz)
    . mapMaybe (view objectWireframe) $ os

prepareRenderObjects' :: ShaderLocations -> IO ()
prepareRenderObjects' locs = do
  depthFunc $= Just Lequal
  currentProgram $= Just (locs ^. programGpuIntersection)

prepareRenderObjects :: ShaderLocations -> IO ()
prepareRenderObjects locs = do
  depthFunc $= Just Lequal
  currentProgram $= Just (locs ^. programOldStyle)

prepareRenderWireframes :: IO ()
prepareRenderWireframes = do
  depthFunc $= Just Always
  currentProgram $= Nothing
