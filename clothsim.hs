import Graphics.Rendering.OpenGL
import Data.Array.IArray
import Data.Array

-- x::Vector3 -> Double
x (Vector3 x y z) = x
y (Vector3 x y z) = y
z (Vector3 x y z) = z
(./.)::Vector3 Double -> Double -> Vector3 Double
(./.) (Vector3 x y z) m = Vector3 (x/m) (y/m) (z/m) 
(.+.) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0 + x1) (y0 + y1) (z0 + z1) -- vectr addition
(.-.) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0 - x1) (y0 - y1) (z0 - z1) -- vector subtraction
(...) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = (x0 * x1) + (y0 * y1) + (z0 * z1) -- dot product
(.*.) (Vector3 x y z) m = Vector3 (x*m) (y*m) (z*m)
-- Vector3 = (fromIntegral x) (fromIntegral y) (fromIntegral z) :: Vector3 GLfloat <- ???
-- particles = [Vector3 x 0 z|  z <- [0.0..9.0], x <- [0.0..9.0]]

addForce::Double -> Vector3 Double -> Vector3 Double
addForce oneOverM (Vector3 x y z) = Vector3 (x*oneOverM) (y*oneOverM) (z*oneOverM)

data Particle = MkParticle { pos :: Vector3 Double, 
                             v :: Vector3 Double, 
                             f :: Vector3 Double, 
                             mass :: Double 
                           } deriving (Show)

data Spring = MkSpring { kd :: Float,
                         ks :: Float,
                         r :: Float,
                         h :: Particle,
                         t :: Particle
                       } deriving (Show)

m = 9
n = 9

m0 = fromIntegral m ::Double
n0 = fromIntegral n ::Double

makeParticles::Double -> Double -> [Particle]
makeParticles m0 n0 = [MkParticle { pos = Vector3  x 0.0 z, v =  Vector3 0.0 0.0 0.0, f =  Vector3 0.0 0.0 0.0, mass = 1.0 } | z <- [0.0..m0], x <- [0.0..n0]]

-- particles = makeParticles (fromIntegral m) (fromIntegral n)

-- a0 = array (0,9) ((0,0.0) : [(i, (fromRational i) ) | i <- [0..9], j <- [0..9]])
particles = makeParticles m0 n0

makeArray::Int -> Int -> [Particle] -> Array Int Particle
makeArray m n l = Data.Array.listArray (0,(m+1)*n) l

-- parArray0 = listArray (1,(m+1)*n) particles
parArray0 = makeArray m n particles

newArr = Data.Array.IArray.amap (x . pos) parArray0

-- integrate::Float -> Array Integer Particle -> Array Integer Particle
-- integrate dt particles = map v 

-- amap f arr = listArray (bounds arr) map f (elems arr)

eulerIntegrate::Array Int Particle -> Double -> Array Int Particle
  
eulerIntegrate ps dt = Data.Array.IArray.amap (`eulerIntegrate'` dt) ps

eulerIntegrate' p dt = MkParticle { pos = position, v = vel, f = Vector3 0.0 0.0 0.0, mass = mass p }
                       where
                         pForce = Vector3 0.0 (-9.8) 0.0
                         oneOverMass = 1.0/(mass p)
                         acc = (pForce ./. oneOverMass) .*. dt
                         vel = ((v p) .+. acc) .*. dt
                         position = (pos p) .+. (v p) 

  --totalForce = pForce + Vector3 0.0 (-9.8) 0.0

