{-# LANGUAGE TemplateHaskell #-}

module Shaders
  ( ShaderLocations
  , programOldStyle
  , programGpuIntersection
  , aNormal, uObjectCenter, uObjectInnerRadius
  , uHyperplaneNormals, uHyperplaneValues, uHyperplaneColors
  , setupShaders
  ) where

import Graphics.Rendering.OpenGL
import Control.Lens
import qualified Data.ByteString as BS
import System.FilePath


data ShaderLocations = ShaderLocations
  { _programOldStyle        :: Program
  , _programGpuIntersection :: Program
  , _aNormal                :: AttribLocation
  , _uObjectCenter          :: UniformLocation
  , _uObjectInnerRadius     :: UniformLocation
  , _uHyperplaneNormals     :: UniformLocation
  , _uHyperplaneValues      :: UniformLocation
  , _uHyperplaneColors      :: UniformLocation
  }

makeLenses ''ShaderLocations


setupShaders :: IO ShaderLocations
setupShaders = do
  program  <- compileShaderProgram
    ["vertshader.sl"]
    ["fragshader.sl", "lighting.sl"]
  program' <- compileShaderProgram
    ["vertshader_gpu_intersection.sl"]
    ["fragshader_gpu_intersection.sl", "lighting.sl"]
  getShaderLocations program program'

compileShaderProgram :: [FilePath] -> [FilePath] -> IO Program
compileShaderProgram vertFiles fragFiles = do
  fragShader <- compileShaderFromFiles VertexShader $
    map (("src" </> "shaders") </>) vertFiles
  vertShader <- compileShaderFromFiles FragmentShader $
    map (("src" </> "shaders") </>) fragFiles
  linkShaderProgram [fragShader, vertShader]

-- Concatenate GLSL code from a list of files and compile it to a shader.
-- This makes line numbers in GLSL error messages less useful,
-- but #include statements in GLSL code would require an extension.
compileShaderFromFiles :: ShaderType -> [FilePath] -> IO Shader
compileShaderFromFiles shaderType files = do
  sources <- mapM BS.readFile files
  shader <- createShader shaderType
  shaderSourceBS shader $= BS.concat sources
  compileShader shader
  success <- compileStatus shader
  case success of
    True -> return shader
    False -> do
      log <- shaderInfoLog shader
      error $ "error compiling shader from files " ++ show files
              ++ "\ninfo log:\n" ++ log

linkShaderProgram :: [Shader] -> IO Program
linkShaderProgram shaders = do
  prog <- createProgram
  attachedShaders prog $= shaders
  linkProgram prog
  success <- linkStatus prog
  case success of
    True -> return prog
    False -> do
      log <- programInfoLog prog
      error $ "error linking shader program\ninfo log:\n" ++ log

getShaderLocations :: Program -> Program -> IO ShaderLocations
getShaderLocations prog prog' = ShaderLocations prog prog'
  <$> get (attribLocation prog "aNormal")
  <*> get (uniformLocation prog "uObjectCenter")
  <*> get (uniformLocation prog "uObjectInnerRadius")
  <*> get (uniformLocation prog' "uHyperplaneNormals")
  <*> get (uniformLocation prog' "uHyperplaneValues")
  <*> get (uniformLocation prog' "uHyperplaneColors")
