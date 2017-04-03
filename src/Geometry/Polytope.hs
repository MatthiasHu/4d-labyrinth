module Polytope
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


-- A convex polytope given as intersetion of half spaces.
type Polytope v a = [Hyperplane v a]

faceVertices :: (SomeScalar a) => Polytope V3 a -> [[V3 a]]
faceVertices planes = undefined
  where
    facesArr = arrayFromList planes
    verts = filter (realVertex facesArr)
          . mapMaybe (mkVertex (facesArr!))
          . triples
          $ indices facesArr

arrayFromList :: [a] -> Array Int a
arrayFromList l = array (1, length l) $ zip [1..] l

type Triple a = [a]
type Pair a = [a]

triples :: [a] -> [Triple a]
triples [] = []
triples (x:xs) = map (x:) (pairs xs) ++ triples xs

pairs :: [a] -> [Pair a]
pairs [] = []
pairs (x:xs) = map (\y -> [x, y]) xs ++ pairs xs

toV3 :: Triple a -> V3 a
toV3 [a, b, c] = V3 a b c
toV3 _ = error "toV3: list is not a triple"

type Vertex a = (Triple Int, V3 a)

mkVertex :: (SomeScalar a) =>
 (Int -> Hyperplane V3 a) -> Triple Int -> Maybe (Vertex a)
mkVertex faces is = if nearZero det then Nothing
  else Just (is, mat' !* vals)
  where
    mat = toV3 . map (view planeNormal . faces) $ is
    mat' = inv33 mat
    det = det33 mat
    vals = toV3 . map (view planeValue . faces) $ is

realVertex :: (SomeScalar a) =>
  Array Int (Hyperplane V3 a) -> Vertex a -> Bool
realVertex facesArr (is, vert) =
    all ((<=0) . flip planeDist (P vert))
  . map (facesArr!)
  . filter (`notElem` is)
  $ indices facesArr
