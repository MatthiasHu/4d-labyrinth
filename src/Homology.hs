module Homology
  ( ChainComplex(..)
  , Homology
  , homology
  , homologyString
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (group, intercalate)

import SparseMatrix
import SmithNormalForm


-- A chain complex consisting of finite free abelian groups.
-- For every x in the set
--   basis !! n,
-- the map
--   boundary x
-- may only contain keys in the set
--   basis !! (n-1).
-- And the chainComplexProperty should be fulfilled.
data ChainComplex a = ChainComplex
  { bases :: [S.Set a]
  , boundary :: a -> M.Map a Integer
  }

chainComplexProperty :: (Ord a) => ChainComplex a -> Bool
chainComplexProperty cc =
  all (all (==0) . extend d . d)
    (foldr S.union S.empty (bases cc))
  where
    d = boundary cc

-- Extend a homomorphism given on basis elements
-- to linear combinations of basis elements.
extend :: (Ord b, Num c) => (a -> M.Map b c) -> M.Map a c -> M.Map b c
extend f lc = foldr (M.unionWith (+)) M.empty
  [ (*c) <$> f a | (a, c) <- M.assocs lc ]

example :: ChainComplex Char
example = ChainComplex
  [S.fromList "ab", S.fromList "cd"]
  ( \c -> case c of
      'c' -> M.fromList [('a', 4)]
      'd' -> M.fromList [('a', 2)] )


homology :: (Ord a) => ChainComplex a -> Homology
homology cc = zipWith3 singleHomology bs diags (tail diags)
  where
    bs = bases cc
    d = boundary cc
    diags = [] :
      map (diagonalForm . flip mkMatrix d) (tail bs)
      ++ [[]]

singleHomology :: S.Set a -> [Integer] -> [Integer] -> GroupType
singleHomology basis diag1 diag2 =
  foldr (M.unionWith (+)) M.empty $ map cyclicGroup moduli
  where
    moduli = diag2 ++ replicate rank 0
    rank = (fromIntegral $ S.size basis) - length diag1 - length diag2

mkMatrix :: (Ord i, Ord j, Eq c, Num c) =>
  S.Set j -> (j -> M.Map i c) -> Matrix i j c
mkMatrix js f = fromEntries
  [ ((i, j), c) | j <- S.toList js, (i, c) <- M.assocs (f j) ]


-- Cyclic group with order a prime power or infinite.
-- These are the "primitive" groups in the classification theorem
-- for finitely generated abelian groups (in prime decomposition form).
data PrimitiveGroupType = ZZ | ZZmod Integer
  deriving (Eq, Ord, Show)

pgtString :: PrimitiveGroupType -> String
pgtString ZZ = "Z"
pgtString (ZZmod n) = "Z"++show n

-- Isomorphism type of a finitely generated abelian group.
-- The direct sum of abelian groups in this representation is simply
-- computed by: M.unionWith (+)
type GroupType = M.Map PrimitiveGroupType Integer

cyclicGroup :: Integer -> GroupType
cyclicGroup 0 = M.singleton ZZ 1
cyclicGroup n = M.fromList [(ZZmod q, 1) | q <- primePowers]
  where
    primePowers = map product . group $ primeFactors n

primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors (-1) = []
primeFactors n = p : primeFactors (n `div` p)
  where
    p = head . filter ((==0) . (n `mod`)) $ [2..]

type Homology = [GroupType]

homologyString :: Homology -> String
homologyString = intercalate "_" . map groupTypeString

groupTypeString :: GroupType -> String
groupTypeString gt = if M.null gt then "0" else
    intercalate "+"
  . map (\(pgt, n) -> intercalate "+" . replicate (fromIntegral n)
                        $ pgtString pgt)
  $ M.assocs gt
