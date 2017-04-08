module Geometry.Polytope
  ( Polytope
  , faceVertices
  ) where

import Linear
import Linear.Affine
import qualified Data.Map as Map
import Control.Lens hiding (indices)
import Data.Array
import Data.Maybe

import Constraints.Scalar
import Geometry.Hyperplane
import Geometry.Combinatorics


-- A convex polytope given as intersetion of half spaces.
type Polytope v a = [Hyperplane v a]

faceVertices :: (SomeScalar a) =>
  (face -> Hyperplane V3 a) -> [face] -> [(face, [Point V3 a])]
faceVertices getHyperplane faces = filter (not . null)
  [ ( facesArr ! p
    , map ((vertsMap Map.!) . putIn p) $ sortPairs (vertsContaining p)
    )
  | p <- faceIDs ]
  where
    facesArr = arrayFromList faces
    faceIDs = indices facesArr
    verts = filter (realVertex getHyperplane facesArr)
          . mapMaybe (mkVertex $ getHyperplane . (facesArr!))
          . triples
          $ faceIDs
    vertsMap = Map.fromList verts
    vertsContaining p = mapMaybe (takeOut p . fst) verts

arrayFromList :: [a] -> Array Int a
arrayFromList l = array (1, length l) $ zip [1..] l

type Vertex a = (Triple Int, Point V3 a)

mkVertex :: (SomeScalar a) =>
 (Int -> Hyperplane V3 a) -> Triple Int -> Maybe (Vertex a)
mkVertex faces is = if nearZero det then Nothing
  else Just (is, P (mat' !* vals))
  where
    mat = toV3 . fmap (view planeNormal . faces) $ is
    mat' = inv33 mat
    det = det33 mat
    vals = toV3 . fmap (view planeValue . faces) $ is

realVertex :: (SomeScalar a) =>
  (face -> Hyperplane V3 a) -> Array Int face -> Vertex a -> Bool
realVertex getHyperplane facesArr (is, vert) =
    all ((<=0) . flip planeDist vert)
  . map (getHyperplane . (facesArr!))
  . filter (`notElem` is)
  $ indices facesArr
