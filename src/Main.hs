module Main where

import SDL hiding (translation)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Lens

import Setup
import Worlds
import Scene
import EventHandling
import Render
import Movement


main :: IO ()
main = do
  (window, shaderLocs) <- setup
  (scene, eye) <- randomTunnel 5
  startTime <- ticks
  let state = State window shaderLocs scene eye startTime
  mainLoop state

mainLoop :: State -> IO ()
mainLoop s = do
  mE <- pollEvent
  case mE of
    Just e -> mainLoopEvent e s
    Nothing -> render s >> mainLoopIdle s

mainLoopEvent :: Event -> State -> IO ()
mainLoopEvent (Event _ payload) =
  handleEvent payload >=> mainLoop

mainLoopIdle :: State -> IO ()
mainLoopIdle s = do
  t <- ticks
  if t >= nextTick
    then tick s >>= mainLoop
    else do
      mE <- waitEventTimeout $ fromIntegral (nextTick - t)
      case mE of
        Just e -> mainLoopEvent e s
        Nothing -> mainLoopIdle s
  where
    nextTick = (s ^. lastTick) + tickInterval

tickInterval :: (Num a) => a
tickInterval = 30

tick :: State -> IO State
tick s0 = do
  let s = over lastTick (+tickInterval) s0
  keydown <- getKeyboardState
  return $ s
    & movementInput speed (keydown ScancodeW) (keydown ScancodeS)
  where
    speed = 0.10

render :: State -> IO ()
render s = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  renderScene (s ^. shaderLocs) $ Transformed (s ^. eye) (s ^. scene)
  glSwapWindow (s ^. window)
