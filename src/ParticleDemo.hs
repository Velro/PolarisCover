{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

module ParticleDemo (main) where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.List as List
import Control.Monad
import Data.Foldable (for_)
import Foreign.C.Types
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

import SpatialMath --for quaternion to euler
import YamMath


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (128 * 6, 80 * 6)

screenWidthInt, screenHeightInt :: Int
(screenWidthInt, screenHeightInt) = (128 * 6, 80 * 6)

maxCamRotationDelta = 15
maxRoll = 80 --must be less than 90

useMouse :: Bool
useMouse = True

data PlayerInputData = PlayerInputData { playerPosition :: L.V2 CFloat -- x , z
                                       , playerRotation :: L.V2 CFloat -- pan , tilt
                                       }

data Player = Player { cam :: U.Camera CFloat --should be Transform composed with a Camera
                      , lastFrameInput :: PlayerInputData
                     }

data VBO = VBO {vertices :: [L.V3 CFloat]
                 ,colors :: [L.V3 CFloat]
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
      "Particle Demo"
      SDL.defaultWindow {SDL.windowInitialSize = L.V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext window

  SDL.setMouseLocationMode (SDL.RelativeLocation)

  let cam = U.dolly (L.V3 15 20 15) $ U.fpsCamera
  let camTwo = U.panGlobal (45) . U.tilt (-45) $ cam
  let initPlayer = Player (camTwo ) (PlayerInputData (L.V2 0 0) (L.V2 0 0))

  resources <- initResources

  gen <- R.getStdGen
  let startParticleSystem = createParticleSystem gen
  Mix.withAudio Mix.defaultAudio 256 $ do
    --music <- Mix.load "Riddlydiddly.wav"
    --Mix.playMusic Mix.Forever music
    loop window initPlayer 0 startParticleSystem resources
    --Mix.free music

  --SDL.setRelativeMouseMode False
  SDL.destroyWindow window
  SDL.quit

--eventually just deltaTime, inputStruct, and entity list?
loop :: SDL.Window -> Player -> CFloat -> ParticleSystem -> U.ShaderProgram -> IO ()
loop window player lastFrameTime particleSystem resources = do
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

  let vbo = genParticleVBO (particles particleSystem)
  glBufferObj <- arrayToBufferObject (vertices vbo)
  draw (Derp resources) glBufferObj vbo updatedPlayer

  SDL.glSwapWindow window
  let newParticleSystem = updateParticleSystem particleSystem deltaTime
  --let debugVerts = vertices mesh
  --print (debugVerts !! 0)
  unless (quit || escapeButtonDown) (loop window updatedPlayer time newParticleSystem resources)



updatePlayer :: PlayerInputData -> Point L.V2 CInt -> Player -> CFloat -> CFloat -> Player
updatePlayer (PlayerInputData (L.V2 moveX moveY) (L.V2 rotateX rotateY)) (P (L.V2 mouseRelX mouseRelY))
              (Player oldCamera unusedB) moveSpeed rotateSpeed =
  Player updatedCam (PlayerInputData (L.V2 moveX moveY) (L.V2 rotateX rotateY)) where
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
  if roll > maxRoll && roll < (180 - maxRoll)
  then False
  else True
  where roll = abs (eRoll (euler321OfQuat (U.orientation camera)) * 57.2957795131)

--how can pattern between smoothedPlayer and v2MoveTowards be represented "the haskell way"

--smoothing delta should eventually be per Axis, bundled in some Input struct
smoothedPlayer :: PlayerInputData -> PlayerInputData -> CFloat -> PlayerInputData
smoothedPlayer (PlayerInputData oldMove oldRotate) (PlayerInputData rawMove rawRotate) maxDelta =
  PlayerInputData resultMove resultRotate where
    resultMove = v2MoveTowards oldMove rawMove maxDelta
    resultRotate = v2MoveTowards oldRotate rawRotate maxDelta

gatherInputsRaw :: (SDL.Scancode -> Bool) -> PlayerInputData
gatherInputsRaw events =
  PlayerInputData (L.V2 updatedMoveX updatedMoveZ) (L.V2 updatedRotateX updatedRotateY) where
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


initResources :: IO U.ShaderProgram
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    U.loadShaderProgram [(GL.VertexShader, "particles.v.glsl"),
                                        (GL.GeometryShader, "particles.g.glsl"),
                                        (GL.FragmentShader, "particles.f.glsl")]


arrayToBufferObject :: [L.V3 CFloat] ->IO GL.BufferObject
arrayToBufferObject arr = U.fromSource GL.ArrayBuffer arr

--gl setup from http://www.geeks3d.com/20140815/particle-billboarding-with-the-geometry-shader-glsl/
draw :: Derp -> GL.BufferObject -> VBO -> Player -> IO ()
draw r glBuffer (VBO vertices colors) player = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
    GL.currentProgram $= (Just . U.program . shaderProgram $ r) --how do i do this without enclosing type?
    U.enableAttrib (shaderProgram r) "coord3d"
    GL.bindBuffer GL.ArrayBuffer $= Just (glBuffer)
    U.setAttrib (shaderProgram r) "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    U.asUniform (mv player) $ U.getUniform (shaderProgram r) "mv"
    U.asUniform (projection screenWidthInt screenHeightInt) $ U.getUniform (shaderProgram r) "projection"
    GL.drawArrays GL.Points 0 (fromIntegral (length vertices))
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "coord3d") $= GL.Disabled

mv :: Player -> L.M44 GL.GLfloat
mv player = view L.!*! model where
  model      = L.mkTransformationMat L.identity $ L.V3 0 0 0 --im not sure if things should be moved here, or on a per vertice levels
  view       = U.camMatrix (cam player)

projection :: Int -> Int -> L.M44 GL.GLfloat
projection width height = U.projectionMatrix (pi/4) aspect 0.1 1000 where
  aspect     = fromIntegral width / fromIntegral height

shaderPath :: FilePath
shaderPath = ""--original path was  wi`kibook" </> "tutorial-05-3D

data Derp = Derp {
  shaderProgram :: U.ShaderProgram
}

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------Particles---------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------

data Particle = Particle { particlePosition :: L.V3 CFloat
                          ,particleVelocity :: L.V3 CFloat
                          ,particleAcceleration :: L.V3 CFloat
                          ,particleColor :: L.V3 CFloat
                          ,particleTimeAlive :: CFloat
                          }
data ParticleSystem = ParticleSystem {
  particles :: [Particle]
  --settings :: ParticleSettings
}

genParticleVBO :: [Particle] -> VBO
genParticleVBO particleArray =
  VBO newVertices newColors where
    newVertices = map particlePosition particleArray--position of each particle
    newColors = map particleColor particleArray--color of each particle

createParticleSystem :: R.StdGen -> ParticleSystem
createParticleSystem gen =
  ParticleSystem array where
    (array, _) = createParticleArrayStart 100 gen
    --array = [createParticle gen | i <-[0..500]]

createParticleArrayStart :: Int -> R.StdGen -> ([Particle], R.StdGen)
createParticleArrayStart maxParticles gen =
  createParticleArray 1 maxParticles [particle] newGen where
  (particle, newGen) = createParticle gen

createParticleArray :: Int -> Int -> [Particle] -> R.StdGen -> ([Particle], R.StdGen)
createParticleArray count maxParticles particles gen =
  if count < maxParticles
  then createParticleArray (count + 1) maxParticles (newParticle:particles) newGen
  else (particles, gen)
  where (newParticle, newGen) = createParticle gen

updateParticleSystem :: ParticleSystem -> CFloat -> ParticleSystem
updateParticleSystem (ParticleSystem oldParticles) deltaTime =
  ParticleSystem newParticleArray where
  newParticleArray = [(updateParticle (oldParticles !! i) deltaTime) | i <- [0..((length oldParticles) -1)] ] --filter for particles that should be killed

updateParticle :: Particle -> CFloat -> Particle
updateParticle (Particle pPosition pVelocity b c pTimeAlive) deltaTime =
  Particle newParticlePosition pVelocity b c newParticleTimeAlive
           where newParticlePosition = pPosition + velocityDelta
                 newParticleTimeAlive = pTimeAlive + deltaTime
                 velocityDelta = v3TimesScalar pVelocity deltaTime

createParticle :: R.StdGen -> (Particle, R.StdGen)
createParticle gen =
  (Particle initPos initVelocity initAcceleration initColor initTimeAlive, gen'') where
  initPos = (randomUnitVectorGen gen) * 5
  (_, gen') = R.next gen
  initVelocity = (randomUnitVectorGen gen') * 1
  (_, gen'') = R.next gen'
  initAcceleration = L.V3 0 0 0
  initColor = L.V3 1 0 1
  initTimeAlive = 0

{-
-- | return vector with random values between -1 and 1
randomUnitVector :: IO (L.V3 CFloat)
randomUnitVector = do
  gen <- R.newStdGen
  let randoms = R.randomRs (-1,1) gen :: [CFloat]
  return L.V3 (randoms !! 0) (randoms !! 1) (randoms !! 2)--there should be a nicer way to do this


-- | random number between 0 and 1
randomValue :: IO CFloat
randomValue = do
  gen <- R.newStdGen
  let (result, _) = R.randomR (0, 1) gen
  return result
-}

-- | return vector with random values between -1 and 1
randomUnitVectorGen :: R.StdGen -> L.V3 CFloat
randomUnitVectorGen gen = randomVector where
  randoms = R.randomRs (-1,1) gen :: [CFloat]
  randomVector = L.V3 (randoms !! 0) (randoms !! 1) (randoms !! 2)--there should be a nicer way to do this


-- | random number between 0 and 1
randomValueGen ::R.StdGen -> CFloat
randomValueGen gen = result where
  (result, _) = R.randomR (0, 1) gen
