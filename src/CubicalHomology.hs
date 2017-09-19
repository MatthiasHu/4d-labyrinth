{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module CubicalHomology
  ( Homology
  , cubicalHomology
  , WithComplement(..)
  , cubicalHomologies
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Data.List (transpose)

import Homology
import Constraints.Vector


data Coord a = Point a | Interval a
  deriving (Eq, Ord, Show, Functor)

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

-- The orthogonal cube (having the codimension).
-- Note that (dualCube . dualCube) is not id,
-- but a shift by 1 in every dimension.
dualCube :: (Num a) => Cube a -> Cube a
dualCube = map dualCoord
  where
    dualCoord (Point x) = Interval x
    dualCoord (Interval x) = Point (x+1)

-- All cubes of dimesion n contained in a given box.
-- (The length of the list of box coordinates determines
-- the ambient dimension.)
boxCubes :: (Enum a, Num a) => Int -> [(a, a)] -> [Cube a]
boxCubes n [] = if n==0 then [[]] else []
boxCubes n ((a, b):rest) = withPoint ++ withInterval
  where
    withPoint = [ (c:cs) | c <- map Point [a..b]
                         , cs <- boxCubes n rest ]
    withInterval = if n>0
      then [ (c:cs) | c <- map Interval [a..b-1]
                    , cs <- boxCubes (n-1) rest ]
      else []

-- All cubes that are boundaries of the given top dimension cubes.
-- Sorted by ascending dimension.
cubicalComplex :: forall v a. (SomeVector v, Ord a, Num a) =>
  [v a] -> [S.Set (Cube a)]
cubicalComplex topCubes = reverse . take (dim+1) $
  iterate (foldMap (S.fromList . boundaryCubes))
    (S.fromList $ map (map Interval . toList) topCubes)
  where
    dim = sum (pure 1 :: v Int)

data WithComplement a = WithComplement
  { original :: a
  , complement :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable)

cubicalChainComplexes :: forall v a.
  (SomeVector v, Ord a, Num a, Enum a) =>
  [v a] -> WithComplement (ChainComplex (Cube a))
cubicalChainComplexes topCubes = WithComplement
  { original = ChainComplex
      { bases = cc
      , boundary = M.fromList . cubeBoundary
      }
  , complement = ChainComplex
      { bases = complementCubicalComplex cc
      , boundary = M.fromList . cubeBoundary
      }
  }
  where
    cc = cubicalComplex topCubes

-- A cubical complex representing the complement of
-- the given cubical complex.
-- The complement is taken with respect to a surrounding box
-- (so that the complement complex is again finite).
complementCubicalComplex :: (Num a, Ord a, Enum a) =>
  [S.Set (Cube a)] -> [S.Set (Cube a)]
complementCubicalComplex cc =
  [ S.fromList [ c | c <- boxCubes n box, dualCube c `S.notMember` cs]
  | (cs, n) <- zip (reverse cc) [0..] ]
  where
    points = if null cc then [] else
      map (map fromPointCoord) . toList . head $ cc
    fromPointCoord (Point x) = x
    fromPointCoord _ = error $
      "complementCubicalComplex got non-point cube in first cube set"
    box = toList $ if null points then pure (0, 0) else
      minMax <$> transpose points
    minMax l = (minimum l - 1, maximum l)

cubicalHomologies :: (SomeVector v, Ord a, Num a, Enum a) =>
  [v a] -> WithComplement Homology
cubicalHomologies = fmap (init . homology) . cubicalChainComplexes

cubicalHomology :: (SomeVector v, Ord a, Num a, Enum a) =>
  [v a] -> Homology
cubicalHomology = original . cubicalHomologies
