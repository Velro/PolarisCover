--TODO
{-
  -getKey, getKeyDown, getKeyUp
      --seems to be dependent on previous state, perhaps this and deltaTime calc can be added to a single Engine.Update call kinda thing?
  -antialiasing
  -render text to screen
  -entity component system
    - maybe do "functional reactive" instead?
    - HMH approach with Flags per entity and do work as needed? He probably revamps this later in stream
  -scene graph
    - scale/rotate/move based on tree structure
    - compress matrix transform
    - recurse through tree from bottom to top?
    - transform, rotate, scale matrix transformation
  -collision
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
