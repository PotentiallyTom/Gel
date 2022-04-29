module Rendering where

import Graphics.Gloss
    ( color, line, pictures, polygon, Path, Picture, Point, light, Color, makeColor )
import MeshData
    ( Edge(start, end),
      Face(colour, boarder),
      Mesh(edges, faces),
      Point3 )
import Data.List ( sortBy )
import GHC.Float (float2Double, int2Float)

--Takes a series of paths, and maps it to a picture that can be rendered
compoundPaths :: [Path] -> Picture 
compoundPaths p = pictures (map line p)

--turns an edge to a path using a specific projection function
edgeToPath ::(Point3 -> Point) -> Edge -> Path 
edgeToPath projectFunction e = map projectFunction [start e, end e]

--goes through all the edges, and maps the points to the right projected position
projectPaths :: (Point3 -> Point) -> [Edge] -> [Path]
projectPaths projectFunction = map (edgeToPath projectFunction)

--The specifichic orthographic rendering functions
--Just rips the x and y values out from the vector
orthographicProject :: Point3 -> Point
orthographicProject (x,y,z) = (x,y)

--technically this could be made to take any project function, but by making it not be, it
--makes it easier to slot in and out of the rest of the program
orthgraphicRender :: Mesh -> Picture
orthgraphicRender m = compoundPaths (projectPaths orthographicProject (edges m))


--The specific Perspective functions
--the scale factor and camdist allow you to tweak the "zoom" and "FOV" of the camera
--In the end, it looks best when these are similar values, so I keep them the same 
--when calling the function
perspectiveProject :: Float -> Float -> Point3 -> Point
perspectiveProject scaleFactor camDist (x,y,z)  = (x * delta, y * delta)
    where
        delta = scaleFactor / min (z - camDist) (-0.00001) --the min is required to prevent the image from inverting when it gets "behind" the camera

perspectiveRender :: Mesh -> Picture 
perspectiveRender m = compoundPaths (projectPaths (perspectiveProject 200 200) (edges m))


--These are the functions used for rendering with faces, rather than rendering with a wireframe
--This computes the average depth of the points making up the face. It's the best estimate I could
--come up with as to whether a given face should be rendered in front of another
computeFaceDepth :: Face -> Float 
computeFaceDepth p = sum (map extractZ $ boarder p) /  int2Float (length (boarder p))

extractZ :: Point3 -> Float 
extractZ (_,_,z) = z

--takes a list of faces, orders them from front to back
orderFaces :: [Face] -> [Face]
orderFaces = sortBy orderFace

--the ordering is required for my naieve implementation of not drawing things behind other things:
--just draw things back to front. As far as I'm aware Orderings are the standard way of sorting
--things in haskell, so I used one
orderFace :: Face -> Face -> Ordering 
orderFace f1 f2
    |f1Depth > f2Depth = GT 
    |f1Depth < f2Depth = LT
    |otherwise = EQ
        where
            f1Depth = computeFaceDepth f1
            f2Depth = computeFaceDepth f2

--There's likely a way of deriving this automatically, but it's such an
--easy function that it's really not worth doing
sumPoint3 :: Point3 -> Point3 -> Point3
sumPoint3 (x,y,z) (x',y',z') = (x + x', y + y', z + z')

--sums a set of vectors
sumVectors :: [Point3] -> Point3 
sumVectors = foldr sumPoint3 (0,0,0) 

--calculates the normal vector by finding the average vector from the origin
--since the magnitude is irrelevant, I don't bother dividing
--This is a staggeringly naieve method for this, and assumes that all normals
--go through the origin. They don't, but it works for platonic solids, and this
--function is easy enogh to swap out with one that uses cross products or whatever
--other method you want to use for finding normals of faces
calculateNormalVector :: Face -> Point3 
calculateNormalVector a = sumVectors $ boarder a

dotProduct :: Point3 -> Point3 -> Float
dotProduct (x,y,z) (x',y',z') = (x*x') + (y*y') + (z*z')

-- abs a
magnitude :: Point3 -> Float
magnitude (x,y,z) = sqrt (x^2 + y^2 + z^2)

--returns the angle in radians between 2 vector3s. Used to calculate whether a face is 
-- looking towards the light source or not
angle3 :: Point3 -> Point3 -> Float
angle3 a b = acos (dotProduct a b / (magnitude a * magnitude b))

--constant used for the lighting, corresponds to the angle of the "sun", magnitude doesn't matter
lightPos :: Point3 
lightPos = (-1,0.3,1)

--Generates a float between 0 and 1 for how bright the face should be shaded
--1 means its bright, 0 dimmer
calculateFaceShading :: Face -> Float
calculateFaceShading a = (3.1416 - angle3 (calculateNormalVector a) lightPos) / (3.1416)

--darkens a colour by factor fac
scaleColor :: Float -> Color -> Color  
scaleColor fac c = c * makeColor fac fac fac 1

--takes a projection function and a face, and creates a shaded-coloured picture of that face
drawSingleFace :: (Point3 -> Point) -> Face -> Picture 
drawSingleFace f a = color (scaleColor (calculateFaceShading a) (colour a)) (polygon (map f $ boarder a))

faceRender :: (Point3 -> Point) -> Mesh -> Picture 
faceRender f m = pictures (map (drawSingleFace f) (orderFaces $ faces m))

