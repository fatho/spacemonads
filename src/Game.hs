{-# LANGUAGE TypeOperators, DataKinds, RecordWildCards #-}

module Game where

import Control.Applicative
--import Control.Arrow
import Control.Lens
import Control.Monad.Random
import Data.Foldable (foldMap, traverse_)
import Data.Traversable (mapM)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import Graphics.Rendering.OpenGL as GL hiding (position)
import Graphics.VinylGL
import System.FilePath ((</>))
import Linear

import Control.Wire hiding ((<+>))
import FRP.Netwire hiding ((<+>))

import Prelude hiding (mapM, (.), id)
import GHC.Ptr

import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Vector.Storable as VS
import qualified  Graphics.UI.GLFW as GLFW

import Framework
import Types
import Logic

-- * app type
-------------------------------------------------------------------
data Asset = EnemiesA | EnemiesB | ATitle | APause | APlayer deriving (Show, Eq, Ord)
data SpaceMonads = SpaceMonads
              { _assets      :: Map.Map Asset TextureObject
              , _shader      :: ShaderProgram
              , _postProcess :: ShaderProgram
              , _invadersVAO :: VAO
              , _shipVAO     :: VAO
              , _genericSprite :: VAO
              , _postProcessFBO :: (FramebufferObject, TextureObject)
              , _shaderSeed :: GLfloat
              }


-- * OpenGL records
-------------------------------------------------------------------
type AppInfo = PlainRec '["cam" ::: M33 GLfloat]
type Pos = "vertexCoord" ::: V2 GLfloat
type Tex = "texCoord"    ::: V2 GLfloat

type TexSampler = "tex" ::: GLint

glPos :: Pos
glPos = Field

glTex :: Tex
glTex = Field

glCam :: "cam" ::: M33 GLfloat
glCam = Field

glTime :: "time" ::: GLfloat
glTime = Field

texSampler :: TexSampler
texSampler = Field

-- * game implementation
-------------------------------------------------------------------
invaderTexCoord :: InvaderType -> [V2 GLfloat]
invaderTexCoord it = V2 <$> [x, x+hx] <*> [y, y+hy] where
  idx = fromEnum it
  (ix, iy) = (idx `mod` 3, idx `div` 3)
  (hx, hy) = (1 / 3, 1 / 2)
  (x, y)   = (hx * fromIntegral ix, hy * fromIntegral iy)

invaderCoords :: Swarm -> VS.Vector (PlainRec [Pos,Tex])
invaderCoords swarm = VS.fromList $ concatMap packInvader (swarm ^. invaders) where
  packInvader inv = zipWith (<+>) (map (glPos =:) $ inv ^. position ^. to invRect) 
                                  (map (glTex =:) $ inv ^. invaderType ^. to invaderTexCoord)
  invRect (V2 x y) = map (fmap realToFrac) $ V2 <$> [x - sx, x + sx] <*> [y - sy, y + sy]
  (sx, sy) = (24, 16)

invaderIndices :: Int -> VS.Vector Word32
invaderIndices n = VS.fromList $ take (n * 6) $ foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]

initialSwarm = Swarm (V2 50 50) [Invader (toEnum j) (V2 (fromIntegral i * 64 + 32) (fromIntegral j * 32 + 32)) | i <- [0..9], j <- [0..5]]

-- * graphic implementation
-------------------------------------------------------------------

squareVertices :: V2 GLfloat -> V2 GLfloat -> VS.Vector (PlainRec [Pos,Tex])
squareVertices (V2 x y) (V2 w h) = VS.fromList $ zipWith (<+>) (map (glPos =:) ps) (map (glTex =:) ts) where
  psr = V2 <$> [x,x+w] <*> [y,y+h]
  tsr = V2 <$> [0,1] <*> [0,1]
  idx = [0,1,2,2,1,3]
  ps  = map (psr!!) idx
  ts  = map (tsr!!) idx

runSpaceMonads :: IO UI -> IO ()
runSpaceMonads updateUIState = 
    updateUIState >>= spaceMonadsInit >>= go clockSession_ mainWire
  where
    go s w state = do
      ui <- updateUIState
      (t, s') <- stepSession s
      (r, w') <- stepWire w t (Right ui)

      case r of
        Left _       -> return ()
        Right scene -> do
          render state ui scene
          go s' w' state

spaceMonadsInit :: UI -> IO SpaceMonads
spaceMonadsInit ui = do
    let assetFiles = Map.fromList 
                  [ (EnemiesA, "enemiesa.png")
                  , (EnemiesB, "enemiesb.png")
                  , (ATitle, "title.png")
                  , (APause, "pause.png")
                  , (APlayer, "player.png")
                  ]
    assets <- mapM loadTextureFile assetFiles
    shader <- loadShaderProgramWith [ (VertexShader, "res"</>"shader"</>"simple.vert")
                                    , (FragmentShader,"res"</>"shader"</>"simple.frag")]
                                    (const (return ()))
    postProcessingShader <- loadShaderProgramWith [ (VertexShader, "res"</>"shader"</>"post.vert")
                                    , (FragmentShader,"res"</>"shader"</>"post.frag")]
                                    (const (return ()))
    setUniforms shader (texSampler =: 0)
    setUniforms postProcessingShader (texSampler =: 0)
    shaderSeed <- getRandom

    -- create VAO for invaders
    verts <- bufferVertices $ invaderCoords initialSwarm
    eb <- bufferIndices (invaderIndices 60)
    invadersVAO <- makeVAO $ do
      enableVertices' shader verts
      bindVertices verts
      bindBuffer ElementArrayBuffer $= Just eb

    -- create VAO for sprite
    spriteVerts <- bufferVertices $ squareVertices (V2 0 0) (V2 1 1)
    spriteVAO <- makeVAO $ do
      enableVertices' shader spriteVerts
      bindVertices spriteVerts

    -- create secondary framebuffer for offscreen rendering
    fboTex <- genObjectName
    withTextures2D [fboTex] $ do
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      texture2DWrap $= (Repeated, ClampToEdge)
      texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (ui ^. windowSize ^. _x ^. to fromIntegral) (ui ^. windowSize ^. _y ^. to fromIntegral)) 0 (PixelData RGBA UnsignedByte nullPtr)
    fbo <- genObjectName
    bindFramebuffer Framebuffer $= fbo
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D fboTex 0
    get (framebufferStatus Framebuffer) >>= \status -> case status of
      Complete -> return ()
      other    -> putStrLn "Framebuffer Status:" >> print other
    bindFramebuffer Framebuffer $= defaultFramebufferObject

    return SpaceMonads
            { _assets = assets
            , _shader = shader
            , _postProcess = postProcessingShader
            , _invadersVAO = invadersVAO
            , _shipVAO     = undefined
            , _genericSprite = spriteVAO
            , _postProcessFBO = (fbo, fboTex)
            , _shaderSeed = shaderSeed
            }

render :: SpaceMonads -> UI -> Scene -> IO ()
render SpaceMonads{..} ui scene = do
  ------------------------------------------------------
  -- activate framebuffer object for offscreen rendering
  let (fbo, fboTex) = _postProcessFBO
  bindFramebuffer Framebuffer $= fbo
  ------------------------------------------------------

  GL.clear [ColorBuffer]--, AccumBuffer, StencilBuffer, DepthBuffer]
  currentProgram $= Just (program _shader)
  let 
    V2 w h = ui ^. windowSize ^. to (fmap fromIntegral)
    camMat = matTranslate (-1) (1) !*! matScale (2 * recip w) (-2 * recip h)

  case scene of
    TitleScene -> do
      setUniforms _shader (glCam =: (matTranslate (-1) (1) !*! matScale 2 (-2)))
      withVAO _genericSprite . withTextures2D [_assets Map.! ATitle] $ drawArrays Triangles 0 6
    PauseScene -> do
      setUniforms _shader (glCam =: (matTranslate (-1) (1) !*! matScale 2 (-2)))
      withVAO _genericSprite . withTextures2D [_assets Map.! APause] $ drawArrays Triangles 0 6
    GameScene ship -> do  
      setUniforms _shader (glCam =: camMat)
      let 
        enemiesA = _assets Map.! EnemiesA
        enemiesB = _assets Map.! EnemiesB
        enemies  = if ceiling (ui ^. currentTime * 2) `mod` 2 == 0 then enemiesA else enemiesB
      withVAO _invadersVAO . withTextures2D [enemies] $ drawIndexedTris 120

      let
        shipMat = matTranslate (ship ^. position . _x . to realToFrac - 26)
                               (ship ^. position . _y . to realToFrac - 16) 
              !*! matScale 52 32

      setUniforms _shader (glCam =: (camMat !*! shipMat))

      withVAO _genericSprite . withTextures2D [_assets Map.! APlayer] $ drawArrays Triangles 0 6


  ------------------------------------------------------
  -- restore framebuffer object
  ------------------------------------------------------
  bindFramebuffer Framebuffer $= defaultFramebufferObject

  GL.clear [ColorBuffer]--, AccumBuffer, StencilBuffer, DepthBuffer]

  -- activate postprocessing shader
  currentProgram $= Just (program _postProcess)
  let shaderTime = _shaderSeed * 1000 + ui ^. currentTime ^. to realToFrac
  setUniforms _postProcess (glCam =: (matTranslate (-1) (-1) !*! matScale 2 2) <+> glTime =: shaderTime)

  withVAO _genericSprite . withTextures2D [fboTex] $ drawArrays Triangles 0 6

  GLFW.swapBuffers

spaceMonadExit :: SpaceMonads-> IO ()
spaceMonadExit app = return ()

matScale :: GLfloat -> GLfloat -> M33 GLfloat
matScale sx sy = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 1)

matTranslate :: GLfloat -> GLfloat -> M33 GLfloat
matTranslate tx ty = V3 (V3 1 0 tx) (V3 0 1 ty) (V3 0 0 1)