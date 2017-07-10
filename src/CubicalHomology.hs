{-# LANGUAGE ScopedTypeVariables #-}

module CubicalHomology
  ( Homology
  , cubicalHomology
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable (toList)

import Homology
import Constraints.Vector


data Coord a = Point a | Interval a
  deriving (Eq, Ord, Show)

-- Hypercubes of possibly lower dimesion than the ambient space.
-- Example: [Point 0, Interval 0, Interval 5] is the sqare with
-- vertices [0, 0, 5], [0, 0, 6], [0, 1, 5], [0, 1, 6].
type Cube a = [Coord a]

cubeBoundary :: (Num a) => Cube a -> [(Cube a, Integer)]
cubeBoundary [] = []
cubeBoundary (Point x:rest) = mapFst (Point x:) $ cubeBoundary rest
cubeBoundary (Interval x:rest) =
  (Point  x   :rest,  1) :
  (Point (x+1):rest, -1) :
  (mapFst (Interval x:) . mapSnd (*(-1)) $ cubeBoundary rest)

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (\(a, b) -> (f a, b))
mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (\(a, b) -> (a, f b))

boundaryCubes :: (Num a) => Cube a -> [Cube a]
boundaryCubes = map fst . cubeBoundary

cubicalChainComplex :: forall v a. (SomeVector v, Ord a, Num a) =>
  [v a] -> ChainComplex (Cube a)
cubicalChainComplex topCubes = ChainComplex
  { bases = cubeSets
  , boundary = M.fromList . cubeBoundary
  }
  where
    cubeSets = reverse . take (dim+1) $
      iterate (foldMap (S.fromList . boundaryCubes))
        (S.fromList $ map (map Interval . toList) topCubes)
    dim = sum ((pure 1) :: v Int)

cubicalHomology :: (SomeVector v, Ord a, Num a) => [v a] -> Homology
cubicalHomology = init . homology . cubicalChainComplex
