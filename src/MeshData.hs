module MeshData where

import Graphics.Gloss
    ( Color, blue, cyan, green, magenta, red, yellow, black, white, greyN)

type Point3 = (Float, Float, Float)

--A 3D vector defined by a start and end point
data Edge = Edge 
    {
        start :: Point3 ,
        end :: Point3 
    } deriving (Eq, Show)

data Face = Face
    {
        --the boarder is a set of 3D points that are eventually projected to 
        --a series of 2D points, which are then converted to a path - one of 
        --Gloss's data types to represent an unbroken line. For this reason,
        --the boarder points must be A: convex, and B: in order (like a joint
        -- the dots puzzel)
        boarder :: [Point3],
        --colour english spelling represents the colour of a face
        --color american spelling represents colours introduced in gloss
        --I'm sure this is going to cause me 0 headaches down the line
        colour :: Color
    } deriving (Eq, Show)

data Mesh = Mesh
    {
        -- Since the edges and faces are independant of each other,
        --It's possible to encode 2 shapes in the same mesh data: one
        --as a wireframe, and the other as a collection of faces.
        edges :: [Edge],
        faces :: [Face]
    } deriving (Eq, Show)


--the names of the points correspond to the corner of the cube, which makes them easier to use
-- later down the line, not actually stored by the mesh data, but storing it here makes it
--a lot easier to write down the same points a bunch of times
p000,p001,p010,p011,p100,p101,p110,p111 :: Point3
p000 = (-70,-70,-70)
p001 = (-70,-70,70)
p010 = (-70,70,-70)
p011 = (-70,70,70)
p100 = (70,-70,-70)
p101 = (70,-70,70)
p110 = (70,70,-70)
p111 = (70,70,70)

--This is the major data that is being renderd.
--By replacing these lines, you can render any 3-d object you want.
cubeFaces :: [Face]
cubeFaces = 
    [
        Face [p000, p001, p011, p010] red,
        Face [p100, p101, p111, p110] green,
        Face [p001, p101, p100, p000] blue,
        Face [p010, p110, p111, p011] yellow,
        Face [p011, p111, p101, p001] cyan,
        Face [p000, p010, p110, p100] magenta 
    ]


--this is the set of edge data. It's not actually seen in the final render function,
--however it was used a lot for debugging, and is used for the wireframe rendering
--functions
cubeEdges :: [Edge]
cubeEdges = 
    [
        Edge p000 p001 ,
            Edge p001 p011 ,
            Edge p001 p101 ,

        Edge p000 p010 ,
            Edge p010 p011 ,
            Edge p010 p110 ,

        Edge p000 p100 ,
            Edge p100 p110 ,
            Edge p100 p101, 
        
        Edge p111 p110,
        Edge p111 p101,
        Edge p111 p011
    ]

--the actual mesh that corresponds to the cube
cubeMesh :: Mesh
cubeMesh = Mesh {edges = cubeEdges,  faces = cubeFaces}

--Some other meshes

dx0, dx1, dy0, dy1, dz0, dz1 :: Point3
dx0 = (-70,0,0)
dx1 = (70,0,0)
dy0 = (0,-70,0)
dy1 = (0,70,0)
dz0 = (0,0,-70)
dz1 = (0,0,70)

d8Faces :: [Face]
d8Faces = 
    [
        Face [dx0, dy0, dz0] red,
        Face [dx0, dy0, dz1] black,
        Face [dx0, dy1, dz0] $ greyN 0.5,
        Face [dx0, dy1, dz1] green,
        Face [dx1, dy0, dz0] yellow,
        Face [dx1, dy0, dz1] magenta,
        Face [dx1, dy1, dz0] cyan,
        Face [dx1, dy1, dz1] blue
    ]

d8Mesh :: Mesh
d8Mesh = Mesh {edges = [], faces = d8Faces}


-- a red cube to show off the lighting engine
redCubeFaces :: [Face]
redCubeFaces = 
    [
        Face [p000, p001, p011, p010] red,
        Face [p100, p101, p111, p110] red,
        Face [p001, p101, p100, p000] red,
        Face [p010, p110, p111, p011] red,
        Face [p011, p111, p101, p001] red,
        Face [p000, p010, p110, p100] red 
    ]

redMesh :: Mesh
redMesh = Mesh {edges = cubeEdges, faces = redCubeFaces}