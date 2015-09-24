--TODO
{-
  -Draw origin coordinates
  -mouse pointer velocity for camera
  -render text to screen
  -audio
  -transparency, blending boxes together
  -scene graph
    - scale/rotate/move based on tree structure
    - compress matrix transform
    - recurse through tree from bottom to top?
-}

--TOLEARN
{-
  -Learn "correct way" to access V2 members
  -maybe switch to Template Haskell or Vinyl implementation
  -understand weird Resources constructor
  -<*> and <$>
  -Reseach LambdaCase, MultiWayIf, and PatternSynonyms pragmas
  -See if current deltaTime calculation could be done with SDL Timers instead,
      like in lazyfoo's c++ example
-}

--Notes
{-
  -mouseX/mouseY = SDL.getMouseLocation
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Monad
import Data.Foldable (for_)
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.Exit (exitFailure)
import           System.IO
import           Control.Applicative
import           System.FilePath ((</>))

import SDL (($=))
import qualified SDL
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import Data.Fixed

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

screenWidthInt, screenHeightInt :: Int
(screenWidthInt, screenHeightInt) = (640, 480)

{- Not necessary until more complex input
--todo switch to some sort of Enum
-- | 2  = isDown
-- | 1  = OnDown
-- | 0  = isUp
-- | -1 = onUp
data MarshalledInputs = MarshalledInputs { arrowUp :: Integer
                                          , arrowDown :: Integer
                                          , arrowLeft :: Integer
                                          , arrowRight :: Integer
                                          , mousePos :: V2 Double
                                          , escape :: Integer  }
-}
data PlayerInputData = PlayerInputData { playerPosition :: V2 CFloat -- x , z
                                       , playerRotation :: V2 CFloat -- pan , tilt
                                       }

data Player = Player { cam :: U.Camera CFloat
                     }


-- | Represents the shader program and its input buffers
data Resources = Resources { shaderProgram :: U.ShaderProgram
                          , vertBuffer :: GL.BufferObject
                          , colorBuffer :: GL.BufferObject
                          , elementBuffer :: GL.BufferObject
                          }--, mesh :: Mesh -- how do we include the mesh in the weird constructor

data Mesh = Mesh {vertices :: [L.V3 Float]
                 ,indices :: [L.V3 GL.GLuint]
                 ,colors :: [L.V3 Float]
                 }


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "Polaris Cover"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext window

  let initPlayer = Player U.fpsCamera

  loop window initPlayer 0

  SDL.destroyWindow window
  SDL.quit


--eventually just deltaTime, inputStruct, and entity list?
loop :: SDL.Window -> Player -> CFloat -> IO ()
loop window player lastFrameTime = do
  time <- SDL.time--maybe we can find this with SDL timers?
  let deltaTime = (time - lastFrameTime) `mod'` 1 --mod by 1 because the second or third frame comes up with a deltaTime in the thousands...
  let moveSpeed = deltaTime * 10
  let rotateSpeed = deltaTime * 30

  let collectEvents = do
        e <- SDL.pollEvent
        case e of
          Nothing -> return []
          Just e' -> (e' :) <$> collectEvents

  events <- map SDL.eventPayload <$> collectEvents
  let quit = SDL.QuitEvent `elem` events
  keyboardEvents <- SDL.getKeyboardState
  let escapeButtonDown = keyboardEvents SDL.ScancodeEscape

  let playerData = gatherInputs keyboardEvents
  let updatedPlayer = updatePlayer playerData player moveSpeed rotateSpeed

  let longMesh = transformMesh cubeMesh (L.V3 10 0 0) (L.V3 1 3 1)
  let masterMesh = concatMesh longMesh cubeMesh
  resources <- initResources masterMesh
  draw resources masterMesh updatedPlayer

  SDL.glSwapWindow window

  unless (quit || escapeButtonDown) (loop window updatedPlayer time)

updatePlayer :: PlayerInputData -> Player -> CFloat -> CFloat -> Player
updatePlayer playerInput player moveSpeed rotateSpeed =
  Player updatedCam where
    xMoveDelta =   realToFrac (v2XAccessor (playerPosition playerInput)* moveSpeed)
    zMoveDelta =   realToFrac (v2YAccessor (playerPosition playerInput)* moveSpeed)
    xRotateDelta = realToFrac (v2XAccessor (-(playerRotation playerInput))) * rotateSpeed
    yRotateDelta = realToFrac (v2YAccessor (playerRotation playerInput))   * rotateSpeed
    newPos = U.rightward (cam player) ^* xMoveDelta
             + U.forward (cam player) ^* zMoveDelta
    updatedCam = U.dolly newPos .
                 U.pan xRotateDelta .
                 U.tilt yRotateDelta $ cam player  --U.pan for left/right rotation, U.tilt for up/down rotation


--smooth inputs vs raw inputs, probably need to wrap this function to do that
gatherInputs :: (SDL.Scancode -> Bool) -> PlayerInputData
gatherInputs  events =
  PlayerInputData (V2 updatedMoveX updatedMoveZ) (V2 updatedRotateX updatedRotateY) where
  updatedMoveX = if | events SDL.ScancodeA -> -1
                    | events SDL.ScancodeD -> 1
                    | otherwise -> 0
  updatedMoveZ = if | events SDL.ScancodeW -> 1
                    | events SDL.ScancodeS -> -1
                    | otherwise -> 0
  updatedRotateX = if | events SDL.ScancodeLeft -> -1
                      | events SDL.ScancodeRight ->1
                      | otherwise -> 0
  updatedRotateY = if | events SDL.ScancodeUp -> 1
                      | events SDL.ScancodeDown -> -1
                      | otherwise -> 0

initResources :: Mesh -> IO Resources
initResources newMesh = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- As our shaders take more inputs, collecting the attributes gets
    -- annoying.  GLUtil helps out with the ShaderProgram type, which
    -- keeps track of the 'AttribLocations' and 'UniformLocation's by
    -- name.
    let v = shaderPath </> "cube.v.glsl"
        f = shaderPath </> "cube.f.glsl"
    Resources <$> U.simpleShaderProgram v f
              <*> U.fromSource GL.ArrayBuffer (vertices newMesh)
              <*> U.fromSource GL.ArrayBuffer (colors newMesh)
              <*> U.fromSource GL.ElementArrayBuffer (indices newMesh)


draw :: Resources -> Mesh -> Player -> IO ()
draw r mesh player = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.depthFunc $= Just GL.Less
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- In C++ example GLUT handles resizing the viewport?
    --(width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

    t <- SDL.time -- time in seconds since program launch
    GL.currentProgram $= (Just . U.program . shaderProgram $ r)
    U.enableAttrib (shaderProgram r) "coord3d"
    U.enableAttrib (shaderProgram r) "v_color"
    GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
    U.setAttrib (shaderProgram r) "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
    U.setAttrib (shaderProgram r) "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    U.asUniform (transformM screenWidthInt screenHeightInt t player) $ U.getUniform (shaderProgram r) "mvp"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer r)
    U.drawIndexedTris (fromIntegral $ length (indices mesh))
    -- GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    -- GLUtil does not yet provide a function to disable attributes
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "coord3d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "v_color") $= GL.Disabled

transformM :: Int -> Int -> Double -> Player -> L.M44 GL.GLfloat
transformM width height t player = projection L.!*! view L.!*! model L.!*! anim where
  angle      = 0 --realToFrac t * pi/4
  anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
  model      = L.mkTransformationMat L.identity $ L.V3 0 0 0 --im not sure if things should be moved here, or on a per vertice levels
  view       = U.camMatrix (cam player)
  projection = U.projectionMatrix (pi/4) aspect 0.1 1000 --last value is draw distance
  aspect     = fromIntegral width / fromIntegral height

shaderPath :: FilePath
shaderPath = ""--original path was  wikibook" </> "tutorial-05-3D

-- This does not result in the same face order as the C++ example.
-- The first 4 vertices correspond to the right (positive X) face.
cubeVertices :: [L.V3 Float]
cubeVertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

cubeColors :: [L.V3 Float]
cubeColors = L.V3 <$> [1, 0] <*> [1, 0] <*> [1, 0] -- color space visualization

-- Vertices for each triangle in CCW order
cubeIndices :: [L.V3 GL.GLuint]
cubeIndices = [ L.V3 2 1 0 -- right
           , L.V3 1 2 3
           , L.V3 0 1 4 -- top
           , L.V3 4 1 5
           , L.V3 4 5 6 -- left
           , L.V3 7 6 5
           , L.V3 2 6 3 -- bottom
           , L.V3 6 3 7
           , L.V3 0 4 2 -- front
           , L.V3 2 4 6
           , L.V3 5 1 7 -- back
           , L.V3 7 1 3
           ]

cubeMesh :: Mesh
cubeMesh = Mesh cubeVertices cubeIndices cubeColors

--maybe map this to (+)
--should take multiple meshes eventually, maybe has type [Mesh] -> Mesh, could be a recursion
concatMesh :: Mesh -> Mesh -> Mesh
concatMesh meshA meshB =
  Mesh newV newI newC where
    newV = vertices meshA ++ vertices meshB
    newI = indices meshA ++ map(+ L.V3 vertCountMeshA vertCountMeshA vertCountMeshA) (indices meshB)
      where vertCountMeshA = fromIntegral $ length (vertices meshA)
    newC = colors meshA ++ colors meshB

transformMesh :: Mesh -> L.V3 Float -> L.V3 Float -> Mesh
transformMesh mesh position scale =
  Mesh newVertices (indices mesh) (colors mesh) where
    newVertices = map((+ position) . (* scale))(vertices mesh)
--v2
v2XAccessor :: L.V2 CFloat -> CFloat
v2XAccessor (L.V2 x _) = x

v2YAccessor :: L.V2 CFloat -> CFloat
v2YAccessor (L.V2 _ y) = y

--v3
v3XAccessor :: L.V3 Float -> Float
v3XAccessor (L.V3 x _ _) = x

v3YAccessor :: L.V3 Float -> Float
v3YAccessor (L.V3 _ y _) = y

v3ZAccessor :: L.V3 Float -> Float
v3ZAccessor (L.V3 _ _ z) = z
