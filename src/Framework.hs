{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}

module Framework where

-- * Imports
-------------------------------------------------------------------

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable (foldMap, traverse_)
import System.FilePath ((</>))
import Data.IORef

import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Linear

import Debug.Trace

import qualified Data.Set as Set

-- * Types
-------------------------------------------------------------------

deriving instance Ord MouseButton

data UI = UI 
          { _pressedKeys :: Set.Set Key
          , _pressedMouseBtns :: Set.Set MouseButton
          , _mousePos :: V2 Int
          , _windowSize :: V2 Int
          , _currentTime :: Double
          , _timeStep :: Double
          } deriving (Show, Eq)

makeLenses ''UI

-- * Main function
-------------------------------------------------------------------

setupGLFW :: String -> Int -> Int -> IO (IO UI)
setupGLFW title w h = do
  GLFW.initialize
  -- general OpenGL settings
  --GLFW.openWindowHint FSAASamples 4
  --GLFW.openWindowHint OpenGLVersionMajor 3
  --GLFW.openWindowHint OpenGLVersionMinor 3
  --GLFW.openWindowHint OpenGLProfile OpenGLCoreProfile
  -- create window
  GLFW.openWindow (Size (fromIntegral w) (fromIntegral h)) [DisplayRGBBits 8 8 8] Window
  GLFW.windowTitle $= title
  GLFW.enableSpecial StickyKey
  -- set nice background color
  GL.clearColor $= Color4 0.0 0.0 0.0 1.0
  -- enable alpha blending
  GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  GL.blend $= Enabled
  -- event initialisation
  keySetIO      <- newIORef Set.empty
  mouseBtnSetIO <- newIORef Set.empty
  mousePosIO    <- get GLFW.mousePos >>= newIORef . posToV2
  windowSizeIO  <- get GLFW.windowSize >>= newIORef . sizeToV2
  lastTimeIO    <- get time >>= newIORef

  GLFW.keyCallback $= onKeyEvent keySetIO
  GLFW.mouseButtonCallback $= onMouseBtnEvent mouseBtnSetIO
  GLFW.mousePosCallback $= onMouseMove mousePosIO
  GLFW.windowSizeCallback $= onWindowResize windowSizeIO
  -- run main loop
  let 
    updateUIState = do
      curTime  <- get time
      lastTime <- readIORef lastTimeIO
      writeIORef lastTimeIO curTime
      UI <$> readIORef keySetIO 
         <*> readIORef mouseBtnSetIO
         <*> readIORef mousePosIO
         <*> readIORef windowSizeIO
         <*> pure curTime
         <*> pure (curTime - lastTime)
  return updateUIState
  

loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do img <- readTexture (".." </> "res" </> f)
                   texFilter
                   return img
        texFilter = do textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
                       texture2DWrap $= (Repeated, ClampToEdge)

loadTextureFile :: FilePath -> IO TextureObject
loadTextureFile f = do 
    img <- readTexture ("res"</>"gfx"</> f)
    texFilter
    return $ either error id img
  where
    texFilter = textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
             >> texture2DWrap $= (Repeated, ClampToEdge)

-- * event handling
-------------------------------------------------------------------

onKeyEvent :: IORef (Set.Set Key) -> Key -> KeyButtonState -> IO ()
onKeyEvent ref k Press   = modifyIORef ref (Set.insert k)
onKeyEvent ref k Release = modifyIORef ref (Set.delete k)

onMouseBtnEvent :: IORef (Set.Set MouseButton) -> MouseButton -> KeyButtonState -> IO ()
onMouseBtnEvent ref k Press   = modifyIORef ref (Set.insert k)
onMouseBtnEvent ref k Release = modifyIORef ref (Set.delete k)

onMouseMove :: IORef (V2 Int) -> Position -> IO ()
onMouseMove ref pos = writeIORef ref (posToV2 pos)

onWindowResize :: IORef (V2 Int) -> Size -> IO ()
onWindowResize ref size = writeIORef ref (sizeToV2 size)

posToV2 :: Integral a => Position -> V2 a
posToV2 (Position x y) = V2 (fromIntegral x) (fromIntegral y)

sizeToV2 :: Size -> V2 Int
sizeToV2 (Size x y) = V2 (fromIntegral x) (fromIntegral y)