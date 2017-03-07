module Main where

import SDL hiding (translation)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Lens
import Data.Monoid

import Setup
import Worlds
import Scene
import EventHandling
import Render
import Transformation


main :: IO ()
main = do
  window <- setup
  (scene, eye) <- randomTunnel 10
  t <- ticks
  let state = State window scene eye t
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

tickInterval = 30

tick :: State -> IO State
tick s0 = do
  let s = over lastTick (+tickInterval) s0
  keydown <- getKeyboardState
  let movement = speed * ( val (keydown ScancodeW)
                         - val (keydown ScancodeS) )
      val True = 1
      val False = 0
  return $ s & over eye (translation (V3 0 0 movement) <>)
  where
    speed = 0.10

render :: State -> IO ()
render s = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  renderScene $ Transformed (s ^. eye) (s ^. scene)
  glSwapWindow (s ^. window)
