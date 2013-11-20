{-# LANGUAGE TypeOperators, DataKinds, RecordWildCards #-}

module Game (runSpaceMonads) where

import Control.Applicative
import Control.Monad hiding (mapM)
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
data Asset = EnemiesA | EnemiesB 
           | ATitle | APause | AGameOver
           | APlayer | APlayerShot | AAlienShot deriving (Show, Eq, Ord)
data SpaceMonads = SpaceMonads
              { _assets       :: Map.Map Asset TextureObject
              , _shader       :: ShaderProgram
              , _postProcess  :: ShaderProgram
              , _invadersGeom :: Geometry
              , _shipVAO      :: VAO
              , _genericSprite :: VAO
              , _postProcessFBO :: (FramebufferObject, TextureObject)
              , _shaderSeed :: GLfloat
              }

data Geometry = Geometry
                { _geomVAO :: VAO
                , _geomVertices :: BufferedVertices [Pos,Tex] 
                , _geomIndices :: BufferObject 
                }

createGeometry :: (BufferSource (w VertexRec), BufferSource (v Word32)) 
              => ShaderProgram -> w VertexRec -> v Word32 -> IO Geometry
createGeometry shader vertices indices = do
  verts <- bufferVertices vertices
  eb <- bufferIndices indices
  vao <- makeVAO $ do
    enableVertices' shader verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb
  return (Geometry vao verts eb)

-- * OpenGL records
-------------------------------------------------------------------
type AppInfo = PlainRec '["cam" ::: M33 GLfloat]
type Pos = "vertexCoord" ::: V2 GLfloat
type Tex = "texCoord"    ::: V2 GLfloat
type VertexRec = PlainRec [Pos,Tex]

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

-- * graphic implementation
-------------------------------------------------------------------

squareVertices :: V2 GLfloat -> V2 GLfloat -> VS.Vector (PlainRec [Pos,Tex])
squareVertices (V2 x y) (V2 w h) = VS.fromList $ zipWith (<+>) (map (glPos =:) ps) (map (glTex =:) ts) where
  psr = V2 <$> [x,x+w] <*> [y,y+h]
  tsr = V2 <$> [0,1] <*> [0,1]
  idx = [0,1,2,2,1,3]
  ps  = map (psr!!) idx
  ts  = map (tsr!!) idx

updateSwarmGeom :: Swarm -> Geometry -> IO ()
updateSwarmGeom swarm geom = do
  reloadVertices (_geomVertices geom) (invaderCoords swarm)

runSpaceMonads :: IO UI -> IO ()
runSpaceMonads updateUIState = 
    updateUIState >>= spaceMonadsInit >>= go clockSession_ mainWire (avgFps 60) 0
  where
    go s w fpsWire i state = do
      ui <- updateUIState
      (t, s') <- stepSession s
      (r, w') <- stepWire w t (Right ui)
      (Right fps, fpsWire') <- stepWire fpsWire t (Right ())
      if i `mod` 60 == 0
        then print fps
        else return ()
      case r of
        Left _       -> return ()
        Right scene -> do
          render state ui scene
          go s' w' fpsWire' (i+1) state

spaceMonadsInit :: UI -> IO SpaceMonads
spaceMonadsInit ui = do
    let assetFiles = Map.fromList 
                  [ (EnemiesA, "enemiesa.png")
                  , (EnemiesB, "enemiesb.png")
                  , (ATitle, "title.png")
                  , (APause, "pause.png")
                  , (APlayer, "player.png")
                  , (APlayerShot, "playershot.png")
                  , (AAlienShot, "enemyshot")
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
    swamGeom <- createGeometry shader [] (invaderIndices 60)

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
            , _invadersGeom = swamGeom
            , _shipVAO     = undefined
            , _genericSprite = spriteVAO
            , _postProcessFBO = (fbo, fboTex)
            , _shaderSeed = shaderSeed
            }

render :: SpaceMonads -> UI -> Scene -> IO ()
render SpaceMonads{..} ui scene = do
    ------------------------------------------------------
    withPostProcessing $ do
    --void $ do
      GL.clear [ColorBuffer]
      currentProgram $= Just (program _shader)

      case scene of
        TitleScene -> do
          setUniforms _shader (glCam =: (matTranslate (-1) (1) !*! matScale 2 (-2)))
          withVAO _genericSprite . withTextures2D [_assets Map.! ATitle] $ drawArrays Triangles 0 6
        PauseScene -> do
          setUniforms _shader (glCam =: (matTranslate (-1) (1) !*! matScale 2 (-2)))
          withVAO _genericSprite . withTextures2D [_assets Map.! APause] $ drawArrays Triangles 0 6
        GameScene ship bullets swarm -> do  
          renderInvaders swarm
          renderShip ship
          mapM_ renderBullet bullets
    ------------------------------------------------------
    GLFW.swapBuffers
  where
    ------------------------------------------------------
    -- CAMERA MATRIX ((0,0), (800,600)) --> ((-1,1), (1,-1))
    ------------------------------------------------------
    camera = let V2 w h = ui ^. windowSize ^. to (fmap fromIntegral)
             in matTranslate (-1) (1) !*! matScale (2 * recip w) (-2 * recip h)
    ------------------------------------------------------
    -- RENDER INVADERS
    ------------------------------------------------------
    renderInvaders swarm = do
      let
        swarmMat = matTranslate (swarm ^. position . _x . to realToFrac)
                               (swarm ^. position . _y . to realToFrac) 
      setUniforms _shader (glCam =: (camera !*! swarmMat))
      let 
        enemiesA = _assets Map.! EnemiesA
        enemiesB = _assets Map.! EnemiesB
        enemies  = case swarm ^. swarmAnim of
          SwarmA -> enemiesA 
          _      -> enemiesB
      updateSwarmGeom swarm _invadersGeom
      withVAO (_geomVAO _invadersGeom) . withTextures2D [enemies] $ drawIndexedTris (2 * swarm ^. invaders . to (fromIntegral . length))
    ------------------------------------------------------
    -- RENDER SHIP
    ------------------------------------------------------
    renderShip ship = do
      let
        shipMat = matTranslate (ship ^. position . _x . to realToFrac - 26)
                               (ship ^. position . _y . to realToFrac - 16) 
              !*! matScale 52 32
      setUniforms _shader (glCam =: (camera !*! shipMat))
      withVAO _genericSprite . withTextures2D [_assets Map.! APlayer] $ drawArrays Triangles 0 6
    ------------------------------------------------------
    -- RENDER BULLETS
    ------------------------------------------------------
    renderBullet (Bullet pos owner) = do
      let
        shipMat = matTranslate (pos ^. _x . to realToFrac - 3)
                               (pos ^. _y . to realToFrac - 6) 
              !*! matScale 6 12
        tex = case owner of
          PlayerBullet -> playerBulletTex
          AlienBullet  -> alienBulletTex
      setUniforms _shader (glCam =: (camera !*! shipMat))
      withVAO _genericSprite . withTextures2D [tex] $ drawArrays Triangles 0 6
    playerBulletTex = _assets Map.! APlayerShot
    alienBulletTex = _assets Map.! APlayerShot
    ------------------------------------------------------
    -- POSTPROCESSING
    ------------------------------------------------------
    withPostProcessing action = do
      -- activate framebuffer object for offscreen rendering
      let (fbo, fboTex) = _postProcessFBO
      bindFramebuffer Framebuffer $= fbo
      -- run action
      action
      -- restore framebuffer object
      bindFramebuffer Framebuffer $= defaultFramebufferObject
      GL.clear [ColorBuffer]--, AccumBuffer, StencilBuffer, DepthBuffer]
      -- activate postprocessing shader
      currentProgram $= Just (program _postProcess)
      -- render offscreen texture to screen
      let shaderTime = _shaderSeed * 1000 + ui ^. currentTime ^. to realToFrac
      setUniforms _postProcess (glCam =: (matTranslate (-1) (-1) !*! matScale 2 2) <+> glTime =: shaderTime)
      withVAO _genericSprite . withTextures2D [fboTex] $ drawArrays Triangles 0 6



spaceMonadExit :: SpaceMonads-> IO ()
spaceMonadExit app = return ()

matScale :: GLfloat -> GLfloat -> M33 GLfloat
matScale sx sy = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 1)

matTranslate :: GLfloat -> GLfloat -> M33 GLfloat
matTranslate tx ty = V3 (V3 1 0 tx) (V3 0 1 ty) (V3 0 0 1)