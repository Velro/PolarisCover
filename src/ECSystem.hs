module ECSystem where

import qualified Data.IntMap as Map
import qualified Linear as L
import qualified Graphics.Rendering.OpenGL as GL


--data ComponentType = Transform | Camera | Player
data Component = Component{componentID :: Int
                          ,componentType :: String
                          }
data EntityRequest = EntityRequest{entityRequestName :: String
                      ,componentRequests :: [String]
                      }

data Entity = Entity {entityName :: String
                      ,entityID :: Int
                      ,components :: [Component]
                     }

firstEntityRequest :: EntityRequest
firstEntity = Entity [(Component 0)()]

data ECSystem = ECSystem{entityList :: Map.IntMap Entity
                          ,transform_CL :: Map.IntMap Transform
                            ,mesh_CL ::Map.IntMap Mesh
                            }

emptySystem :: ECSystem
emptySystem = ECSystem Map.empty Map.empty Map.empty

initialSystemWithOneEntity :: EntityRequest -> ECSystem
initialSystemWithOneEntity startEntity = addEntity (emptySystem)

addEntity :: ECSystem -> EntityRequest -> ECSystem
--------------------
-- TRANSFORM
------------------

data Transform = Transform {position :: L.V3 Float --inherits from base Component?
                           ,scale :: L.V3 Float
                           }deriving Show

-- insertTransform :: Map.IntMap Transform -> Int -> Transform -> Map.IntMap Transform
-- insertTransform transformList index newTransform = Map.insert index newTransform transformList

randomTransform :: Transform
randomTransform = Transform (L.V3 1 2 3) (L.V3 2 3 4)

  --------------------
  -- MESH
  ------------------

data Mesh = Mesh {vertices :: [L.V3 Float]
                ,indices :: [L.V3 GL.GLuint]
                ,colors :: [L.V3 Float]
                }

cubeVertices :: [L.V3 Float]
cubeVertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

monochromeColorArray :: L.V3 Float -> [L.V3 Float]
monochromeColorArray (L.V3 r g b) = L.V3 <$> [r, r] <*> [g, g] <*> [b, b] -- color space visualization

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
