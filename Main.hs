module Main where

import SDL
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Lens

import Setup
import Worlds
import Scene
import EventHandling
import Render


main :: IO ()
main = do
  window <- setup
  (scene, eye) <- randomTunnel 10
  let state = State window scene eye
  mainLoop state

mainLoop :: State -> IO ()
mainLoop s = do
  mE <- pollEvent
  case mE of
    Just e -> mainLoopEvent e s
    Nothing -> do
      render s
      e <- waitEvent
      mainLoopEvent e s

mainLoopEvent :: Event -> State -> IO ()
mainLoopEvent (Event _ payload) =
  handleEvent payload >=> mainLoop

render :: State -> IO ()
render s = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  renderScene $ Transformed (s ^. eye) (s ^. scene)
  glSwapWindow (s ^. window)
