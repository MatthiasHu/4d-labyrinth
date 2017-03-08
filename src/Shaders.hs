module Shaders
  ( setupShaders
  ) where

import Graphics.Rendering.OpenGL
import Data.ByteString as BS
import System.FilePath


setupShaders :: IO ()
setupShaders = do
  fragShader <- compileShaderFile VertexShader $
    "src" </> "shaders" </> "vertshader.sl"
  vertShader <- compileShaderFile FragmentShader $
    "src" </> "shaders" </> "fragshader.sl"
  linkShaderProgram [fragShader, vertShader]

compileShaderFile :: ShaderType -> FilePath -> IO Shader
compileShaderFile shaderType file = do
  source <- BS.readFile file
  shader <- createShader shaderType
  shaderSourceBS shader $= source
  compileShader shader
  success <- compileStatus shader
  case success of
    True -> return shader
    False -> do
      log <- shaderInfoLog shader
      error $ "error compiling shader from file " ++ show file
              ++ "\ninfo log:\n" ++ log

linkShaderProgram :: [Shader] -> IO ()
linkShaderProgram shaders = do
  prog <- createProgram
  attachedShaders prog $= shaders
  linkProgram prog
  success <- linkStatus prog
  case success of
    True -> currentProgram $= Just prog
    False -> do
      log <- programInfoLog prog
      error $ "error linking shader program\ninfo log:\n" ++ log
