{-# LANGUAGE TemplateHaskell #-}

module Shaders
  ( ShaderLocations
  , programOldStyle
  , programCpuIntersection
  , aNormal, uObjectCenter, uObjectInnerRadius
  , setupShaders
  ) where

import Graphics.Rendering.OpenGL
import Control.Lens
import Data.ByteString as BS
import System.FilePath


data ShaderLocations = ShaderLocations
  { _programOldStyle        :: Program
  , _programCpuIntersection :: Program
  , _aNormal                :: AttribLocation
  , _uObjectCenter          :: UniformLocation
  , _uObjectInnerRadius     :: UniformLocation
  }

makeLenses ''ShaderLocations


setupShaders :: IO ShaderLocations
setupShaders = do
  program  <- compileShaderProgram "vertshader.sl" "fragshader.sl"
  program' <- compileShaderProgram
    "vertshader_gpu_intersection.sl" "fragshader_gpu_intersection.sl"
  getShaderLocations program program'

compileShaderProgram :: FilePath -> FilePath -> IO Program
compileShaderProgram vert frag = do
  fragShader <- compileShaderFile VertexShader $
    "src" </> "shaders" </> vert
  vertShader <- compileShaderFile FragmentShader $
    "src" </> "shaders" </> frag
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
