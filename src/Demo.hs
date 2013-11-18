{-# LANGUAGE TypeOperators, DataKinds, RecordWildCards #-}

module Demo where

import Control.Applicative
import Control.Lens
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import System.FilePath ((</>))
import Linear

import Debug.Trace

import qualified Data.Map as Map

import Graphics.UI.GLFW as GLFW

import Framework

-- * app type
-------------------------------------------------------------------
data Asset = Grass | Dirt deriving (Show, Eq, Ord)
data DemoApp = DemoApp 
              { _assets :: Map.Map Asset TextureObject
              , _shader :: ShaderProgram
              , _grassVAO, _dirtVAO :: VertexArrayObject
              }


-- * OpenGL records
-------------------------------------------------------------------
type AppInfo = PlainRec '["cam" ::: M33 GLfloat]
type Pos = "vertexCoord" ::: V2 GLfloat
type Tex = "texCoord"    ::: V2 GLfloat

type TexSampler = "tex" ::: GLint

pos :: Pos
pos = Field

tex :: Tex
tex = Field

texSampler :: TexSampler
texSampler = Field

-- * game implementation
-------------------------------------------------------------------
gameLevel :: [Int]
gameLevel = [3,3,3,4,5] --,4,3,3,3,4,5,5,6,7,6,6,6,7,6,5,4,3,3]

tile :: Int -> [V2 GLfloat]
tile h = let h' = fromIntegral h / 10 in V2 <$> [0,0.2] <*> [h', h' - 0.2]

spaceColumns :: [[V2 GLfloat]] -> [[V2 GLfloat]]
spaceColumns = zipWith (map . (_x +~)) [0, 0.2 ..]

tileTex :: [[V2 GLfloat]] -> [PlainRec [Pos,Tex]]
tileTex = foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
  where coords = map (tex =:) $ V2 <$> [0,1] <*> [0,1]

grassTiles :: IO (BufferedVertices [Pos,Tex])
grassTiles = bufferVertices . tileTex . spaceColumns $ map tile gameLevel

dirtTiles :: IO (BufferedVertices [Pos,Tex])
dirtTiles = bufferVertices . tileTex . spaceColumns $ map col gameLevel
  where col :: Int -> [V2 GLfloat]
        col h = foldMap tile [h - 1, h - 2 .. 1]

-- * graphic implementation
-------------------------------------------------------------------

demoApp = GLFWApp 
  { appInit = demoInit
  , appLoop = demoLoop
  , appExit = demoExit
  }

demoInit :: IO DemoApp
demoInit = do
    [grass, dirt] <- loadTextures ["ground.png", "ground_dirt.png"]
    shader <- loadShaderProgramWith [ (VertexShader, ".."</>"shader"</>"simple.vert")
                                    , (FragmentShader,".."</>"shader"</>"simple.frag")]
                                    (const (return ()))
    setUniforms shader (texSampler =: 0)

    grassVerts <- grassTiles
    eb <- bufferIndices (traceShow inds inds)
    grassVAO <- makeVAO $ do
      enableVertices' shader grassVerts
      bindVertices grassVerts
      bindBuffer ElementArrayBuffer $= Just eb
    dirtVerts <- dirtTiles
    dirtVAO <- makeVAO $ do 
      enableVertices' shader dirtVerts
      bindVertices dirtVerts
      bindBuffer ElementArrayBuffer $= Just eb

    return DemoApp 
            { _assets = Map.fromList [(Grass, grass), (Dirt, dirt)]
            , _shader = shader
            , _grassVAO = grassVAO, _dirtVAO = dirtVAO
            }
  where
    inds = take (sum $ map (*6) gameLevel) $
             foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]

demoLoop :: DemoApp -> UI -> IO Bool
demoLoop DemoApp {..} ui = do
    currentProgram $= Just (program _shader)
    setUniforms _shader (cam =: eye3)
    let 
      grass = _assets Map.! Grass
      dirt  = _assets Map.! Dirt
    withVAO _grassVAO . withTextures2D [grass] $ drawIndexedTris numGrassTris
    withVAO _dirtVAO . withTextures2D [dirt] $ drawIndexedTris numDirtTris

    return True
  where 
    numGrassTris = fromIntegral $ 2 * length gameLevel
    numDirtTris = fromIntegral . sum $ map (*2) gameLevel
    cam = Field :: "cam" ::: M33 GLfloat

demoExit :: DemoApp -> IO ()
demoExit app = return ()