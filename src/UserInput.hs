module UserInput where

import MeshData ( Mesh, cubeMesh, d8Mesh, redMesh )
import Graphics.Gloss.Interface.Pure.Game
    ( Event(EventKey), Key(Char), KeyState(Down) )
import Transform
    ( rotateXY, rotateXZ, rotateYZ, scaleAll, transformMesh )


takeInput :: Event -> Mesh -> Mesh
--3 axis rotations
takeInput (EventKey (Char 'w') Down _ _)  m = transformMesh (rotateYZ 0.2) m
takeInput (EventKey (Char 's') Down _ _)  m = transformMesh (rotateYZ (-0.2))  m

takeInput (EventKey (Char 'd') Down _ _)  m = transformMesh (rotateXZ 0.2) m
takeInput (EventKey (Char 'a') Down _ _)  m = transformMesh (rotateXZ (-0.2)) m

takeInput (EventKey (Char 'q') Down _ _)  m = transformMesh (rotateXY 0.2) m
takeInput (EventKey (Char 'e') Down _ _)  m = transformMesh (rotateXY (-0.2)) m

--scale points in all axis
takeInput (EventKey (Char 'z') Down _ _)  m = transformMesh (scaleAll 1.1) m
takeInput (EventKey (Char 'x') Down _ _)  m = transformMesh (scaleAll (1 / 1.1)) m

takeInput (EventKey (Char '1') Down _ _)  _ = cubeMesh
takeInput (EventKey (Char '2') Down _ _)  _ = redMesh
takeInput (EventKey (Char '3') Down _ _)  _ = d8Mesh 
--looks like the id function is useful after all
takeInput _ m = m