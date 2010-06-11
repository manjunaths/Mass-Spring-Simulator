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
         
-- Add in glut libraries
import Graphics.UI.GLUT

data Vec3 v = MkVec3 GLfloat GLfloat GLfloat deriving Show
-- getx::Vec3 -> GLfloat
getx::Vec3 GLfloat -> GLfloat
getx (MkVec3 x y z) = x

gety::Vec3 GLfloat -> GLfloat
gety (MkVec3 x y z) = y

getz::Vec3 GLfloat -> GLfloat
getz (MkVec3 x y z) = z

vabs::Vec3 GLfloat -> GLfloat
vabs (MkVec3 x y z) = sqrt ((x*x) + (y*y) + (z*z))

(./.)::Vec3 GLfloat -> GLfloat -> Vec3 GLfloat
(./.) a m = MkVec3 ((getx a)/m) ((gety a)/m) ((getz a)/m)

-- vector add
(.+.)::Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat
(.+.) a0 a1 = MkVec3 (x0 + x1) (y0 + y1) (z0 + z1) 
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

(.-.)::Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat
(.-.) a0 a1 = MkVec3 (x0 - x1) (y0 - y1) (z0 - z1) -- vector subtraction
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

dot::Vec3 GLfloat -> Vec3 GLfloat -> GLfloat
dot a0 a1 = (x0 * x1) + (y0 * y1) + (z0 * z1) -- dot product
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0
                x1 = getx a1
                y1 = gety a1
                z1 = getz a1

(.*.)::Vec3 GLfloat -> GLfloat -> Vec3 GLfloat 
(.*.) a0 m = MkVec3 (x0*m) (y0*m) (z0*m)
              where
                x0 = getx a0
                y0 = gety a0
                z0 = getz a0

cross::Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat
cross a b = MkVec3 (a2 * b3 - a3 * b2) (-(a1 * b3 - a3 * b1)) (a1 * b2 - a2 * b1)
             where
               a1 = getx a
               a2 = gety a
               a3 = getz a
               b1 = getx b
               b2 = gety b
               b3 = getz b

              