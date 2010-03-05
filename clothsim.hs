import Graphics.Rendering.OpenGL

-- x::Vector3 -> Double
x (Vector3 x y z) = x
y (Vector3 x y z) = y
z (Vector3 x y z) = z
(./.)::Double -> Vector3 Double -> Vector3 Double
(./.) m (Vector3 x y z) = Vector3 (x/m) (y/m) (z/m) 

-- Vector3 = (fromIntegral x) (fromIntegral y) (fromIntegral z) :: Vector3 GLfloat <- ???
particles = [Vector3 x 0 z|  z <- [0.0..9.0], x <- [0.0..9.0]]

addForce::Double -> Vector3 Double -> Vector3 Double
addForce oneOverM (Vector3 x y z) = Vector3 (x*oneOverM) (y*oneOverM) (z*oneOverM)

data Particle = MkParticle { pos :: Vector3 Double, v :: Vector3 Double, f :: Vector3 Double, oneOverM :: Double } deriving (Show)

[MkParticle  {pos= Vector3 x 0 z, f = Vector3 0 0 0, v = Vector3 0 0 0, oneOverM = 1/1.0} | z <- [0.0..9.0], x <- [0.0..9.0]]
-- intergate::[Particle] -> [Particle]
-- integrate source dt = map addForce 
  