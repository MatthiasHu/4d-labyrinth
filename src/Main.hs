{-# LANGUAGE ConstraintKinds #-}

module Main where

import SDL hiding (translation)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Lens
import Data.Monoid ((<>))

import Setup
import State
import Worlds
import RotationMethods
import Transformation
import Scene
import EventHandling
import Render
import Movement
import Transformation
import Constraints.Vector


type Dim v = R4 v

main :: IO ()
main = do
  let world = randomTunnel 5
      rotationMethod = rot4dQuaternion
  (window, shaderLocs) <- setup
  (scene, eye) <- world
  startTime <- ticks
  mainLoop $
    State window shaderLocs scene eye rotationMethod startTime 0

mainLoop :: (SomeVector v, Dim v) =>
  State v -> IO ()
mainLoop s = do
  mE <- pollEvent
  case mE of
    Just e -> mainLoopEvent e s
    Nothing -> render s >> mainLoopIdle s

mainLoopEvent :: (SomeVector v, Dim v) =>
  Event -> State v -> IO ()
mainLoopEvent (Event _ payload) =
      handleEvent payload
  >=> (return . (inactiveFor .~ 0))
  >=> mainLoop

mainLoopIdle :: (SomeVector v, Dim v) =>
  State v -> IO ()
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

tick :: (SomeVector v, Dim v) =>
  State v -> IO (State v)
tick s = do
  keydown <- getKeyboardState
  return (tickPure keydown s)

tickPure :: (SomeVector v, R3 v) =>
  (Scancode -> Bool) -> State v -> State v
tickPure keydown s = s
  & over lastTick (+ tickInterval)
  & rotateIfInactive
  & (movementInput speed $
       map (uncurry compare . over both keydown) movementKeys)
  & (over inactiveFor (+ 1))
  where
    speed = 0.07

rotateIfInactive :: (SomeVector v, R3 v) =>
  State v -> State v
rotateIfInactive s =
  if (s ^. inactiveFor >= 300)
  then s & over eye (rot <>)
  else s
  where
    rot = (s ^. rotationMethod . inactiveRotation) 0.01

movementKeys =
  [ (ScancodeD, ScancodeA)
  , (ScancodeSpace, ScancodeLShift)
  , (ScancodeS, ScancodeW)
  , (ScancodeE, ScancodeQ)
  ]

render :: (SomeVector v, Dim v) => State v -> IO ()
render s = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  renderScene (s ^. shaderLocs) $
    Transformed (totter (fromIntegral $ s ^. lastTick) <> (s ^. eye))
      (s ^. scene)
  glSwapWindow (s ^. window)

totter :: (SomeVector v, Dim v, Floating a) => a -> Transformation v a
totter t = translation (sin (0.001*t) *^ e1)
  where
    e1 = zero & _w .~ 0.01
