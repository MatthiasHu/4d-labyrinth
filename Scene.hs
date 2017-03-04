module Scene
  ( Scene(..)
  , sceneObjects
  ) where

import Data.Monoid


-- A scene tree, managing the order of transformation compositions
-- when listing the transformed objects.
data Scene t a =
    SceneObject a
  | SceneFork [Scene t a]
  | Transformed t (Scene t a)

sceneObjects :: (Monoid t) => Scene t a -> [(t, a)]
sceneObjects = go mempty

go :: (Monoid t) => t -> Scene t a -> [(t, a)]
go t (SceneObject a)    = return (t, a)
go t (SceneFork l)      = concatMap (go t) l
go t (Transformed t' s) = go (t <> t') s
