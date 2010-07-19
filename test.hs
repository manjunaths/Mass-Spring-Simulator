import Graphics.UI.GLUT
import Vector
import Quaternion
import Debug.Trace
import System.IO

update::GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Quaternion
update w h dx dy x y = incr 
  where
    small = (min w h) / 2.0
    offset = MkVec3 (w/2.0) (h/2.0) 0
    a = MkVec3 (x-dx) (h - (y+dy)) 0
    b = MkVec3  x (h - y) 0
    a0 = (trace ("a = " ++ show a) a .-. trace ("offset = " ++ show offset) offset) ./. trace ("small = " ++ show small) small
    -- a0 = (a .-. offset) ./. small
    -- b0 = (b .-. offset) ./. small
    b0 =  (trace ("b = " ++ show b) b .-. offset) ./. trace ("small = " ++ show small) small
    tmpscale = 1.0     
    a1 = MkVec3 (getx a0) (gety a0) (2.0 ** ((-0.5) * (vabs ((trace ("vabs a0 = " ++ show (vabs a0)) ) a0)))) -- (vabs (trace "a0 = " ++ a0 )
    b1 = MkVec3 (getx b0) (gety b0) (2.0 ** ((-0.5) * (vabs ((trace ("vabs b0 = " ++ show (vabs b0)) ) b0)))) -- (vabs (trace "b0 = " ++ b0 ) 
    a2 = a1 ./. trace ("vabs a1 = " ++ show (vabs a1)) (vabs a1)
    b2 = b1 ./. trace ("vabs b1 = " ++ show (vabs b1)) (vabs b1)
    a_cross_b = cross (trace ("vabs a2 = " ++ show (vabs a2)) a2) (trace ("vabs b2 = " ++ show (vabs b2)) b2)
    axis = (trace ("a_cross_b = " ++ show a_cross_b) a_cross_b) ./. (vabs a_cross_b)
    angle = acos (dot a2 b2)
    -- tmp_incr = make_quat (getx axis) (gety axis) (getz axis) angle
    -- tmp_incr = make_quat (trace ("axis = " ++ show axis) axis) (trace ("angle = " ++ show angle) angle)
    tmp_incr = MkQuat axis angle
    incr = if dx == 0 && dy == 0
           then MkQuat (MkVec3 0.0  1.0  0.0) 0.0
           else trace ("tmp_incr = " ++ show tmp_incr) tmp_incr
                

make_quat :: Vec3 -> GLfloat -> Quaternion
make_quat v angle = if sq_norm <= 10e-6
                        then MkQuat (MkVec3 0.0 0.0 0.0) 1.0
                        else MkQuat (MkVec3 (sin_theta * x) (sin_theta * y) (sin_theta * z)) (cos theta)
                      where 
                        x = getX v
                        y = getY v
                        z = getZ v
                        sq_norm = x*x + y*y + z*z
                        theta = angle * 0.5
                        sin_theta = sin theta


main = do
  inh <- readFile "values1.txt" ReadMode
  (dx::GLfloat, dy::GLfloat, x::GLfloat, y::GLfloat) <-hGetContents inh
  -- writeFile "what.txt" (map update )
  print dx dy x y