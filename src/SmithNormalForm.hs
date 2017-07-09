module SmithNormalForm
  ( diagonalForm
  ) where

import qualified Data.Map.Strict as M
import Data.List ((\\))

import SparseMatrix


-- Euclidean algorithm for Bezout representation of the gcd.
-- euclidean a b = (c, d, a', b') must satisfy:
--   (1) (c*a + d*b)*a' = a
--   (2) (c*a + d*b)*b' = b
--   (3) a*(-b') + b*a' = 0
--   (4) c*a' + d*b' = 1.
--   (5) if a divides b then c = 1 and d = 0
-- where (3) follows from (1) and (2),
-- and (4) follows from (1) and (2) if at least one of a and b is
-- nonzero.
euclidean :: (Integral a) => a -> a -> (a, a, a, a)
euclidean 0 b = (0, 1, 0, 1)
euclidean a b = (d - c*s, c, b', a' + s*b')
  where
    (s, r) = divMod b a
    -- b = s*a + r
    (c, d, a', b') = euclidean r a
    -- gcd = c*r + d*a
    -- gcd*a' = r
    -- gcd*b' = a

gcd' :: (Integral a) => a -> a -> a
gcd' a b = c*a + d*b
  where
    (c, d, _, _) = euclidean a b


-- Non-zero diagonal entries of some diagonal matrix equivalent to the
-- given matrix. This is not quite the Smith normal form, but there is
-- not much missing.
diagonalForm :: (Ord i, Ord j, Integral a) => Matrix i j a -> [a]
diagonalForm m
  | M.null (rowsRep m)  = []
  | otherwise           = a : diagonalForm m'
  where
    (i:_) = M.keys (rowsRep m)
    (j:_) = M.keys (rowsRep m M.! i)
    (a, m') = eliminate i j m

-- The matrix must have a non-zero entry at the given position.
-- Returns a matrix with zero row i and zero column j.
eliminate :: (Ord i, Ord j, Integral a) =>
  i -> j -> Matrix i j a -> (a, Matrix i j a)
eliminate i j m
  | M.keys (getRow i m) == [j] && M.keys (getCol j m) == [i]
    = (getEntry (i, j) m, setEntry (i, j) 0 m)
  | M.keys (getCol j m) == [i]
    = let (a, m') = eliminate j i (transpose m)
      in (a, transpose m')
  | otherwise
    = eliminate i j m'
  where
    (i':_) = M.keys (getCol j m) \\ [i]
    a = getEntry (i, j) m
    b = getEntry (i', j) m
    (c, d, a', b') = euclidean a b
    aa = getRow i m
    bb = getRow i' m
    m' = setRow i (c*|aa |+| d*|bb) . setRow i' ((-b')*|aa |+| a'*|bb) $ m

(*|) :: (Num a) => a -> M.Map k a -> M.Map k a
(*|) = fmap . (*)
infixl 7 *|

(|+|) :: (Ord k, Num a) => M.Map k a -> M.Map k a -> M.Map k a
(|+|) = M.unionWith (+)
infixl 6 |+|
