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

faceVertices :: (SomeScalar a) => Polytope V3 a -> [[V3 a]]
faceVertices planes =
  [ map ((vertsMap Map.!) . putIn p) $ sortPairs (vertsContaining p)
  | p <- faceIDs ]
  where
    facesArr = arrayFromList planes
    faceIDs = indices facesArr
    verts = filter (realVertex facesArr)
          . mapMaybe (mkVertex (facesArr!))
          . triples
          $ faceIDs
    vertsMap = Map.fromList verts
    vertsContaining p = mapMaybe (takeOut p . fst) verts

arrayFromList :: [a] -> Array Int a
arrayFromList l = array (1, length l) $ zip [1..] l

type Vertex a = (Triple Int, V3 a)

mkVertex :: (SomeScalar a) =>
 (Int -> Hyperplane V3 a) -> Triple Int -> Maybe (Vertex a)
mkVertex faces is = if nearZero det then Nothing
  else Just (is, mat' !* vals)
  where
    mat = toV3 . fmap (view planeNormal . faces) $ is
    mat' = inv33 mat
    det = det33 mat
    vals = toV3 . fmap (view planeValue . faces) $ is

realVertex :: (SomeScalar a) =>
  Array Int (Hyperplane V3 a) -> Vertex a -> Bool
realVertex facesArr (is, vert) =
    all ((<=0) . flip planeDist (P vert))
  . map (facesArr!)
  . filter (`notElem` is)
  $ indices facesArr
