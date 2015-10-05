--TODO
{-
  -getKey, getKeyDown, getKeyUp
      --seems to be dependent on previous state, perhaps this and deltaTime calc can be added to a single Engine.Update call kinda thing?
  -antialiasing
  -clamp vertical rotation. Currently I can rotate a full 360 vertically
  -render text to screen
  -entity component system
  -scene graph
    - scale/rotate/move based on tree structure
    - compress matrix transform
    - recurse through tree from bottom to top?
    - transform, rotate, scale matrix transformation
  -reinstall packages with profiling support https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
-}

--TOLEARN
{-
  -maybe switch to Template Haskell or Vinyl implementation
  -understand weird Resources constructor
  -<*> and <$>
  -Reseach LambdaCase, MultiWayIf, and PatternSynonyms pragmas
  -See if current deltaTime calculation could be done with SDL Timers instead,
      like in lazyfoo's c++ example
-}

--Entity Component System Notes
{-
-maybe store all components of a certain type in a hashmap,
 entities store a list of keys to their respective components.
 This would in theory make things like collision, scene graph, and mesh building
 more cache friendly over storing the data per entity. AOS vs SOA

So I could have Transform component, Mesh, Renderer, ParticleSystem,
Collision, Camera etc. All of these need to use data from one another. Can an entity be some sort of partially evaluated function?
I think it'd be okay if when these entities needed data from other entities they were looking at what happened last frame. An old
copy of the entity. How do I get a List of all of the Transforms and update them in a collision pass or scenegraph pass without
having to pass the entire entity?
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
import Data.List as List
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
import qualified SDL.Mixer as Mix
import Graphics.Rendering.OpenGL as GL
--import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import Data.Fixed

import qualified System.Random as R

import SpatialMath

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (128 * 3, 80 * 3)

screenWidthInt, screenHeightInt :: Int
(screenWidthInt, screenHeightInt) = (128 * 3, 80 * 3)

{- Not necessary until more complex input
--this seems like a good fit for FRP
--TODO switch to some sort of Enum
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

boxCount = 150
boxRange = 5
colorRangeA = L.V3 1 1 1
colorRangeB = L.V3 0 0 0.5
boxScaleA = L.V3 1 1 1
boxScaleB = L.V3 0.3 0.3 0.3

maxCamRotationDelta = 15
maxRoll = 80 --must be less than 90

useMouse :: Bool
useMouse = True

data PlayerInputData = PlayerInputData { playerPosition :: V2 CFloat -- x , z
                                       , playerRotation :: V2 CFloat -- pan , tilt
                                       }

data Player = Player { cam :: U.Camera CFloat --should be Transform composed with a Camera
                      , lastFrameInput :: PlayerInputData
                      , playerCameraPosition :: L.V3 CFloat
                      , panRotation :: CFloat
                      , tiltRotation :: CFloat
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

data Transform = Transform {t_position :: V3 CFloat
                           ,t_rotation :: Quaternion CFloat --should this be a higher precision?
                           ,t_scale :: V3 CFloat
                           }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
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

  SDL.setRelativeMouseMode True


  let cam = U.dolly (L.V3 15 20 15) $ U.fpsCamera
  let camTwo = U.panGlobal (45) . U.tilt (-45) $ cam
  let initPlayer = Player (camTwo ) (PlayerInputData (L.V2 0 0) (L.V2 0 0)) (L.V3 0 0 0) 45 (-45)

  masterMesh <- generateScene
  Mix.withAudio Mix.defaultAudio 256 $ do
    music <- Mix.load "Riddlydiddly.wav"
    Mix.playMusic Mix.Forever music
    loop window initPlayer 0 masterMesh
    Mix.free music

  SDL.setRelativeMouseMode False
  SDL.destroyWindow window
  SDL.quit

--eventually just deltaTime, inputStruct, and entity list?
loop :: SDL.Window -> Player -> CFloat -> Mesh -> IO ()
loop window player lastFrameTime mesh = do
  time <- SDL.time--maybe we can find this with SDL timers?
  let deltaTime = (time - lastFrameTime) `mod'` 1 --mod by 1 because the second or third frame comes up with a deltaTime in the thousands...
  let moveSpeed = deltaTime * 10
  let rotateSpeed = deltaTime * 20

  let collectEvents = do
        e <- SDL.pollEvent
        case e of
          Nothing -> return []
          Just e' -> (e' :) <$> collectEvents

  events <- map SDL.eventPayload <$> collectEvents
  let quit = SDL.QuitEvent `elem` events

  mouseCoord <- SDL.getRelativeMouseLocation

  keyboardEvents <- SDL.getKeyboardState
  let escapeButtonDown = keyboardEvents SDL.ScancodeEscape
  let reloadButtonDown = keyboardEvents SDL.ScancodeR

  let rawInputs = gatherInputsRaw keyboardEvents
  let smoothPlayer = smoothedPlayer (lastFrameInput player) rawInputs (deltaTime*0.5)
  let updatedPlayer = updatePlayer smoothPlayer mouseCoord player moveSpeed rotateSpeed

  resources <- initResources mesh
  draw resources mesh updatedPlayer

  --print (eRoll (euler321OfQuat (U.orientation (cam updatedPlayer))) * 57.2957795131)
  let roll = abs (eRoll (euler321OfQuat (U.orientation (cam updatedPlayer))) * 57.2957795131)
  if (roll > 80 && roll < 100)
  then print ("roll")
  else print roll

  SDL.glSwapWindow window
  newMesh <- shouldUpdateMesh reloadButtonDown mesh
  unless (quit || escapeButtonDown) (loop window updatedPlayer time newMesh)


shouldUpdateMesh :: Bool -> Mesh -> IO Mesh
shouldUpdateMesh isDown oldMesh =
  if isDown == True
  then do masterMesh <- generateScene; return masterMesh
  else return oldMesh

updatePlayer :: PlayerInputData -> Point L.V2 CInt -> Player -> CFloat -> CFloat -> Player
updatePlayer (PlayerInputData (L.V2 moveX moveY) (L.V2 rotateX rotateY)) (P (L.V2 mouseRelX mouseRelY))
              (Player oldCamera unusedB oldPos oldPan oldTilt) moveSpeed rotateSpeed =
  Player updatedCam (PlayerInputData (L.V2 moveX moveY) (L.V2 rotateX rotateY)) newPos newPan newTilt where
    (L.V2 clampedMouseRelX clampedMouseRelY) = v2ClampMagnitude (L.V2 (fromIntegral mouseRelX) (fromIntegral mouseRelY)) maxCamRotationDelta
    xMoveDelta =   realToFrac (moveX * moveSpeed)
    zMoveDelta =   realToFrac (moveY * moveSpeed)
    newPan = if useMouse
                   then ((-clampedMouseRelX) * rotateSpeed)
                   else realToFrac (-rotateX) * rotateSpeed
    newTilt = if useMouse
                   then ((-clampedMouseRelY) * rotateSpeed)
                   else realToFrac rotateY * rotateSpeed
    newPos = U.rightward (oldCamera) * xMoveDelta
           + U.forward   (oldCamera) * zMoveDelta
           --(U.location oldCamera)
    --movedCam =
    updatedCam = if clampCamera tiltedCamera
                 then tiltedCamera
                 else nonTiltedCamera
                 where tiltedCamera = U.dolly newPos . U.panGlobal newPan . U.tilt newTilt $ oldCamera
                       nonTiltedCamera = U.dolly newPos . U.panGlobal newPan $ oldCamera


clampCamera :: U.Camera CFloat -> Bool
clampCamera camera =
  if (roll > maxRoll && roll < (180 - maxRoll))
  then False
  else True
  where roll = abs (eRoll (euler321OfQuat (U.orientation (camera))) * 57.2957795131)

--how can pattern between smoothedPlayer and v2MoveTowards be represented "the haskell way"

--smoothing delta should eventually be per Axis, bundled in some Input struct
smoothedPlayer :: PlayerInputData -> PlayerInputData -> CFloat -> PlayerInputData
smoothedPlayer (PlayerInputData oldMove oldRotate) (PlayerInputData rawMove rawRotate) maxDelta =
  PlayerInputData resultMove resultRotate where
    resultMove = v2MoveTowards oldMove rawMove maxDelta
    resultRotate = v2MoveTowards oldRotate rawRotate maxDelta

--zip (toList (L.V3 1 2 3)) (toList (L.V3 4 5 6)) to create [(1,7),(2,8),(3,9)]
v2MoveTowards :: (Num a, Ord a) => L.V2 a -> L.V2 a -> a -> L.V2 a
v2MoveTowards (V2 lastValueX lastValueY) (V2 targetValueX targetValueY) maxDelta =
  L.V2 resultX resultY where
    resultX = moveTowards lastValueX targetValueX maxDelta
    resultY = moveTowards lastValueY targetValueY maxDelta

moveTowards :: (Num a, Ord a) => a -> a -> a -> a
moveTowards lastValue target maxDelta =
  let
    sign = signum target
    result = lastValue + (maxDelta * sign)
  in
    if abs result > abs target
    then target
    else result

--smooth inputs vs raw inputs, probably need to wrap this function to do that
gatherInputsRaw :: (SDL.Scancode -> Bool) -> PlayerInputData
gatherInputsRaw events =
  PlayerInputData (V2 updatedMoveX updatedMoveZ) (V2 updatedRotateX updatedRotateY) where
  --positional
  updatedMoveX = if | events SDL.ScancodeA -> -1
                    | events SDL.ScancodeD -> 1
                    | otherwise -> 0
  updatedMoveZ = if | events SDL.ScancodeW -> 1
                    | events SDL.ScancodeS -> -1
                    | otherwise -> 0
  --rotational
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
    GL.multisample $= GL.Enabled
    --GL.depthFunc $= Just GL.Less
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

monochromeColorArray :: L.V3 Float -> [L.V3 Float]
monochromeColorArray (L.V3 r g b) = L.V3 <$> [r, r] <*> [g, g] <*> [b, b] -- color space visualization

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
cubeMesh = Mesh cubeVertices cubeIndices (monochromeColorArray (L.V3 0 0 0))

--maybe map this to (+)
--should take multiple meshes eventually, maybe has type [Mesh] -> Mesh, could be a recursion

concatMesh :: Mesh -> Mesh -> Mesh
concatMesh meshA meshB =
  Mesh newV newI newC where
    newV = vertices meshA ++ vertices meshB
    newI = indices meshA ++ map(+ L.V3 vertCountMeshA vertCountMeshA vertCountMeshA) (indices meshB)
      where vertCountMeshA = fromIntegral $ length (vertices meshA)
    newC = colors meshA ++ colors meshB

transformMesh :: Mesh -> L.V3 Float -> L.V3 Float -> L.V3 Float -> Mesh
transformMesh mesh position scale color =
  Mesh newVertices (indices mesh) (monochromeColorArray color) where
    newVertices = map((+ position) . (* scale))(vertices mesh)


generateScene :: IO Mesh
generateScene = do
  initBox <- createBox
  meshes <- createBoxes initBox boxCount
  return meshes

createBoxes :: Mesh -> Int -> IO Mesh
createBoxes ongoingMesh index = do
    box <- createBox
    let fullMesh = concatMesh ongoingMesh box
    if index > 0
    then createBoxes fullMesh (index - 1)
    else return fullMesh

createBox :: IO Mesh
createBox = do
  cubePosition <- randomUnitVector
  colorLerpValue <- randomValue
  boxScaleValue <- randomValue
  let lerpedColor = L.lerp colorLerpValue colorRangeA colorRangeB
  let lerpedScale = L.lerp boxScaleValue boxScaleA boxScaleB
  let mesh = transformMesh cubeMesh (cubePosition * boxRange) lerpedScale lerpedColor
  return mesh

--return vector with random values between -1 and 1
randomUnitVector :: IO (L.V3 Float)
randomUnitVector = do
  gen <- R.newStdGen
  let randoms = R.randomRs (-1,1) gen :: [Float]
  return (L.V3 (randoms !! 0) (randoms !! 1) (randoms !! 2))--there should be a nicer way to do this
  --return (L.V3 (V.fromList randoms))

--random number between 0 and 1
randomValue :: IO Float
randomValue = do
  gen <- R.newStdGen
  let (result, _) = R.randomR (0, 1) gen
  return result

--mapRange :: Num -> Num -> Num -> Num -> Num -> Num
--mapRange rangeA rangeB rangeX rangeY value =

--v2ComponentClamp :: (Num a, Ord a) => V2 a -> a -> V2 a
--v2ComponentClamp = (V2 inX inY) mx = V2 resultX resultY where
--  resultX = clampValue


v2ClampMagnitude ::(Num a, Ord a, Fractional a, Floating a) => L.V2 a -> a -> L.V2 a
v2ClampMagnitude input mx =
  if vectorLength > mx
  then input * (L.V2 compon compon)
  else input
  where
    vectorLength = v2InnerProduct input input
    compon = (mx / sqrt vectorLength)

clampValue :: (Ord a) => a -> a -> a -> a
clampValue mn mx = max mn . min mx

v2InnerProduct :: (Num a) => L.V2 a -> L.V2 a -> a
v2InnerProduct (V2 aX aY) (V2 bX bY) = aX * bX + aY * bY
