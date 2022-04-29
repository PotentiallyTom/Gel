module Transform where

import MeshData ( Edge(..), Face(..), Mesh(..), Point3 )

--These functions convert a function that applies to Point3s, and applies it 
--to Edges or Faces instead, this reduces complexity since dealing with Points is 
--a lot simpler than dealing with edges, (thanks first class functions)
applyToEdge :: (Point3 -> Point3) -> (Edge -> Edge)
applyToEdge f e = Edge (f $ start e) (f $ end e)

applyToFace :: (Point3 -> Point3) -> (Face -> Face)
applyToFace f a = Face (map f $ boarder a) (colour a)

--These functions rotate a point about the 2 dimensions given
rotateXY :: Float -> Point3 -> Point3 
rotateXY theta (x,y,z) = (newX, newY, z)
    where 
        newX = x * cos(theta) - y * sin(theta)
        newY = y * cos(theta) + x * sin(theta)

rotateYZ :: Float -> Point3 -> Point3 
rotateYZ theta (x,y,z) = (x, newY, newZ)
    where 
        newY = y * cos(theta) - z * sin(theta)
        newZ = z * cos(theta) + y * sin(theta)

rotateXZ :: Float -> Point3 -> Point3 
rotateXZ theta (x,y,z) = (newX, y, newZ)
    where 
        newX = x * cos(theta) - z * sin(theta)
        newZ = z * cos(theta) + x * sin(theta)

--Scales in the global x,y,z. This means that if you've rotated the shape,
-- it probably won't do what you think it does, for this reason I've decided
--not to allow the user to access these functions, as they mostly just screw
--up the viewport
scaleX :: Float -> Point3 -> Point3 
scaleX factor (x,y,z) = (x * factor, y,z)

scaleY :: Float -> Point3 -> Point3 
scaleY factor (x,y,z) = (x, y * factor,z)

scaleZ :: Float -> Point3 -> Point3 
scaleZ factor (x,y,z) = (x, y, z * factor)

--this one is allowed as it doesn't screw things up
scaleAll :: Float -> Point3 -> Point3 
scaleAll factor (x,y,z) = (factor * x, factor * y, factor * z)


--The polymorphic transform function, give this the function you actually want
-- and it will apply it to the whole mesh
transformMesh :: (Point3 -> Point3) -> Mesh -> Mesh 
transformMesh f m = Mesh {edges = map (applyToEdge f) (edges m), faces = map (applyToFace f) (faces m)}
