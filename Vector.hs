module Vector 
       (
         Vec3(..),
         getx,
         gety,
         getz,
         vabs,
         (./.),
         (.+.),
         (.-.),
         dot,
         (.*.),
         cross
         ) where
         
data Vec3 v = MkVec3 Double Double Double deriving Show
-- getx::Vec3 -> Double
getx::Vec3 Double -> Double
getx (MkVec3 x y z) = x

gety::Vec3 Double -> Double
gety (MkVec3 x y z) = y

getz::Vec3 Double -> Double
getz (MkVec3 x y z) = z

vabs::Vec3 Double -> Double
vabs (MkVec3 x y z) = sqrt ((x*x) + (y*y) + (z*z))

(./.)::Vec3 Double -> Double -> Vec3 Double
(./.) a m = MkVec3 ((getx a)/m) ((gety a)/m) ((getz a)/m)

-- vector add
(.+.)::Vec3 Double -> Vec3 Double -> Vec3 Double
(.+.) a0 a1 = MkVec3 (x0 + x1) (y0 + y1) (z0 + z1) 
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

(.-.)::Vec3 Double -> Vec3 Double -> Vec3 Double
(.-.) a0 a1 = MkVec3 (x0 - x1) (y0 - y1) (z0 - z1) -- vector subtraction
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

dot::Vec3 Double -> Vec3 Double -> Double
dot a0 a1 = (x0 * x1) + (y0 * y1) + (z0 * z1) -- dot product
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

(.*.)::Vec3 Double -> Double -> Vec3 Double 
(.*.) a0 m = MkVec3 (x0*m) (y0*m) (z0*m)
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0

cross::Vec3 Double -> Vec3 Double -> Vec3 Double
cross a b = MkVec3 (a2 * b3 - a3 * b2) (-(a1 * b3 - a3 * b1)) (a1 * b2 - a2 * b1)
             where
               a1 = getx a
               a2 = gety a
               a3 = getz a
               b1 = getx b
               b2 = gety b
               b3 = getz b

              