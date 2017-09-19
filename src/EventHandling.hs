module EventHandling
  ( handleEvent
  ) where

import SDL
import Control.Lens
import Data.Monoid
import Data.Int

import State
import Editing
import RotationMethods
import Blockworld
import Homology
import Transformation
import Setup
import Color
import Constraints.Vector


handleEvent :: (SomeVector v, R3 v) =>
  EventPayload -> State v -> IO (State v)
handleEvent (WindowClosedEvent _) = const quitAndExit
handleEvent (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) =
  handleKeyPressed keysym
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ _ d)) =
  handleMouseMotion d
handleEvent (MouseWheelEvent (MouseWheelEventData _ _ d _)) =
  handleMouseWheel d
handleEvent (MouseButtonEvent
  (MouseButtonEventData _ Pressed _ button _ _)) =
  handleMouseButtonPressed button
handleEvent (WindowSizeChangedEvent _) =
  handleWindowSizeChanged
handleEvent _ = return

handleKeyPressed :: Keysym -> State v -> IO (State v)
handleKeyPressed (Keysym _ KeycodeEscape _) _ = quitAndExit
handleKeyPressed _ s = return s

handleMouseMotion :: (SomeVector v) =>
  V2 Int32 -> State v -> IO (State v)
handleMouseMotion (V2 dx dy) = return . rotationInput
  ( fromIntegral dx    * sensitivity
  , fromIntegral (-dy) * sensitivity
  , 0
  )
  where
    sensitivity = 0.005

handleMouseWheel :: (SomeVector v) =>
  V2 Int32 -> State v -> IO (State v)
handleMouseWheel (V2 _ dy) = return . rotationInput
  (0, 0, fromIntegral dy * sensitivity)
  where
    sensitivity = 0.05

rotationInput :: (SomeVector v) =>
  RotationInput Scalar -> State v -> State v
rotationInput input s = s & over eye
  ((s ^. rotationMethod) input <>)

handleMouseButtonPressed :: (SomeVector v, R3 v) =>
  MouseButton -> State v -> IO (State v)
handleMouseButtonPressed ButtonRight s = do
  let s' = removeBlockHere s
  printHomology s'
  return s'
handleMouseButtonPressed ButtonLeft s = do
  color <- randomColor
  let s' = addBlockHere color s
  printHomology s'
  return s'

printHomology :: (SomeVector v) =>
  State v -> IO ()
printHomology s = do
  putStr "object: "
  putStrLn h
  putStr "complement: "
  putStrLn h'
  where
    WithComplement h h' = fmap homologyString . blockworldHomologies $
      s ^. blockworld

handleWindowSizeChanged :: State v -> IO (State v)
handleWindowSizeChanged s = do
  setViewport (s ^. window)
  return s
