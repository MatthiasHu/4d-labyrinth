{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geometry.Combinatorics
  ( choose
  , Triple
  , Pair
  , triples
  , pairs
  , toV3
  , takeOut
  , putIn
  , sortPairs
  ) where

import Linear
import Control.Lens
import qualified Data.Set as Set
import Data.Foldable
import Data.Maybe


-- choose n elements of the list
-- and return them together with the rest of the list
choose :: Int -> [a] -> [([a], [a])]
choose 0 xs = [([], xs)]
choose n [] = []
choose n (x:xs) =
     map (over _1 (x:)) (choose (n-1) xs)
  ++ map (over _2 (x:)) (choose n     xs)

-- choose' n l = map fst $ choose n l
-- but this is more efficient:
choose' :: Int -> [a] -> [[a]]
choose' 0 xs = [[]]
choose' n [] = []
choose' n (x:xs) = map (x:) (choose' (n-1) xs)
                   ++ choose' n xs


newtype Triple a = Triple { unTriple :: [a] }
  deriving (Show, Eq, Ord, Functor, Foldable)

newtype Pair a   = Pair   { unPair   :: [a] }
  deriving (Show, Eq, Ord, Functor, Foldable)

triples :: [a] -> [Triple a]
triples = map Triple . choose' 3

pairs :: [a] -> [Pair a]
pairs = map Pair . choose' 2

toV3 :: Triple a -> V3 a
toV3 (Triple [a, b, c]) = V3 a b c
toV3 _ = error "toV3: list is not a triple"


takeOut :: (Eq a) => a -> Triple a -> Maybe (Pair a)
takeOut x (Triple [a, b, c])
  | x == a    = Just $ Pair [b, c]
  | x == b    = Just $ Pair [a, c]
  | x == c    = Just $ Pair [a, b]
  | otherwise = Nothing

putIn :: (Ord a) => a -> Pair a -> Triple a
putIn x (Pair [a, b])
  | x <= a    = Triple [x, a, b]
  | x <= b    = Triple [a, x, b]
  | otherwise = Triple [a, b, x]

otherOne :: (Eq a) => a -> Pair a -> Maybe a
otherOne x (Pair [a, b])
  | x == a    = Just b
  | x == b    = Just a
  | otherwise = Nothing


-- Sort a list of pairs forming a cycle:
-- (a, b), (b, c), (c, d), ..., (z, a)
-- Returns the empty list if the list does not represent such a cycle.
sortPairs :: (Ord a) => [Pair a] -> [Pair a]
sortPairs [] = []
sortPairs (p@(Pair [a, b]) : rest) =
  fromMaybe [] $ (p:) <$> go a (Set.fromList rest)
  where
    go x ps | x == b     = Just []
            | otherwise  = do
      p <- find (isJust . otherOne x) ps
      (p:) <$> go (fromJust $ otherOne x p) (Set.delete p ps)
