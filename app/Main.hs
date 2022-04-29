module Main where
import Graphics.Gloss ( white, play, Display(InWindow), Picture )

import MeshData 
import Rendering ( perspectiveProject , faceRender, orthographicProject, orthgraphicRender) 
import Transform ()
import UserInput ( takeInput )


--defines the constants for the window
width, height, offset :: Int
width = 500
height = 500
offset = 100

--defines the window 
window :: Display
window = InWindow "QWE / ASD / ZX to manipulate" (width, height) (offset, offset)

--Feel free to use any of the meshes here
object :: Mesh
--object = d8Mesh
--object = cubeMesh
object = redMesh

--The rendering algorithm used to project the 3D data to a 2d image. 
--facerender can be used to make a projection method also draw faces
--Orthographic render just draws the wireframe and was mostly used for debugging
engine :: Mesh -> Picture 
engine = faceRender $ perspectiveProject 300 300
--engine = faceRender orthographicProject
--engine = orthgraphicRender


--orthographic
main :: IO()
main = play window white 30 object engine takeInput (const id) 