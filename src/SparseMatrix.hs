module SparseMatrix
  ( Matrix(rowsRep, colsRep)
  , nullMatrix
  , getEntry
  , setEntry
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable (foldl')


data Matrix i j a = Matrix
  { rowsRep :: M.Map i (M.Map j a)
  , colsRep :: M.Map j (M.Map i a)
  }
  deriving (Eq)

valid :: (Ord i, Ord j, Num a, Eq a) => Matrix i j a -> Bool
valid (Matrix rR cR) =
  (   M.fromList [ ((i, j), a)
                 | (i, row) <- M.assocs rR, (j, a) <- M.assocs row ]
   == M.fromList [ ((i, j), a)
                 | (j, col) <- M.assocs cR, (i, a) <- M.assocs col ]
  ) &&
  all (all (/=0)) rR

nullMatrix :: (Num a) => Matrix i j a
nullMatrix = Matrix M.empty M.empty

transpose :: Matrix i j a -> Matrix j i a
transpose (Matrix rR cR) = Matrix cR rR

fwd :: (Ord k) => a -> k -> M.Map k a -> a
fwd = M.findWithDefault

getRow :: (Ord i) => i -> Matrix i j a -> M.Map j a
getRow i = fwd M.empty i . rowsRep

getCol :: (Ord j) => j -> Matrix i j a -> M.Map i a
getCol j = getRow j . transpose

getEntry :: (Ord i, Ord j, Num a) => (i, j) -> Matrix i j a -> a
getEntry (i, j) = fwd 0 j . getRow i

setEntry :: (Ord i, Ord j, Num a, Eq a) =>
  (i, j) -> a -> Matrix i j a -> Matrix i j a
setEntry (i, j) a (Matrix rR cR)
  | a/=0  = Matrix
    { rowsRep = M.insertWith M.union i (M.singleton j a) rR
    , colsRep = M.insertWith M.union j (M.singleton i a) cR
    }
  | otherwise  = Matrix
    { rowsRep = M.update (delete' j) i rR
    , colsRep = M.update (delete' i) j cR
    }
  where
    delete' k m = let m' = M.delete k m in 
      if M.null m' then Nothing else Just m'

setRow :: (Ord i, Ord j, Num a, Eq a) =>
  i -> M.Map j a -> Matrix i j a -> Matrix i j a
setRow i r m = foldl' (\m' j -> setEntry (i, j) (fwd 0 j r) m') m
  (M.keysSet r `S.union` M.keysSet (getRow i m))

setCol :: (Ord i, Ord j, Num a, Eq a) =>
  j -> M.Map i a -> Matrix i j a -> Matrix i j a
setCol j c = transpose . setRow j c . transpose
