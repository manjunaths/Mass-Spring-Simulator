module Quaternion
       (
       Quaternion(..),
       conjugate,
       quatMul,
       quatPointMul,
       quatRot,
       quat_mul
       ) where

import Graphics.UI.GLUT
import Vector

data Quaternion = MkQuat {getV :: Vec3, getW :: GLfloat} deriving Show

conjugate::Quaternion -> Quaternion
conjugate q = MkQuat ((getV q) .*. (-1.0)) (getW q)

quatScalarMul::Quaternion -> GLfloat -> Quaternion
quatScalarMul q0 s = MkQuat ((getV q0) .*. s) (s * getW q0)

quatAdd::Quaternion -> Quaternion -> Quaternion
quatAdd q0 q1 = MkQuat (getV q0 .+. getV q1) (getW q0 + getW q1)
  
nlerp::Quaternion -> Quaternion -> GLfloat -> Quaternion
nlerp q0 q1 a2 = let w1 = 1.0 - a2
                     qt0 = quatScalarMul q0 w1
                     qt1 = quatScalarMul q1 a2
                 in
                    quatAdd qt0 qt1
quatMul::Quaternion -> Quaternion -> Quaternion
quatMul q1 q2 = let v1 = getV q1
                    v2 = getV q2
                    a1 = getX v1
                    b1 = getY v1
                    c1 = getZ v1
                    d1 = getW q1
                    a2 = getX v2
                    b2 = getY v2
                    c2 = getZ v2
                    d2 = getW q2
                    v = MkVec3 (a1*b2 + b1*a2 + c1*d2 - (d1*c2)) (a1*c2 - (b1*d2) + c1*a2 + d1*b2)  (a1*d2 + b1*c2 - (c1*b2) + d1*a2)
                    w =  a1*a2 - (b1*b2) - (c1*c2) - (d1*d2)
                in
                 MkQuat v w

quatPointMul::Quaternion -> Quaternion -> Vec3
quatPointMul q1 q2 = let v1 = getV q1
                         v2 = getV q2
                         a1 = getX v1
                         b1 = getY v1
                         c1 = getZ v1
                         d1 = getW q1
                         a2 = getX v2
                         b2 = getY v2
                         c2 = getZ v2
                         d2 = getW q2
                         v = MkVec3 (a1*b2 + b1*a2 + c1*d2 - (d1*c2)) (a1*c2 - (b1*d2) + c1*a2 + d1*b2)  (a1*d2 + b1*c2 - (c1*b2) + d1*a2)
                     in
                      v
quat_mul::Quaternion -> Quaternion -> Quaternion
quat_mul ql qr = let v1 = getV ql
                     v2 = getV qr
                     qlx = getX v1
                     qly = getY v1
                     qlz = getZ v1
                     qlw = getW ql
                     qrx = getX v2
                     qry = getY v2
                     qrz = getZ v2
                     qrw = getW qr
                     w = qlw * qrw - qlx * qrx - qly * qry - qlz * qrz
                     x = qlw * qrx + qlx * qrw + qly * qrz - qlz * qry
                     y = qlw * qry + qly * qrw + qlz * qrx - qlx * qrz
                     z = qlw * qrz + qlz * qrw + qlx * qry - qly * qrx
                     v = MkQuat (MkVec3 x y z) w
                     in v
                     

-- --quaternion multiplication
-- function quat_mult(Quaternion q1, Quaternion q2)
-- {
--         result.a = (q1.a*q2.a -q1.b*q2.b -q1.c*q2.c -q1.d*q2.d)
--         result.b = (q1.a*q2.b +q1.b*q2.a +q1.c*q2.d -q1.d*q2.c)
--         result.c = (q1.a*q2.c -q1.b*q2.d +q1.c*q2.a +q1.d*q2.b)
--         result.d = (q1.a*q2.d +q1.b*q2.c -q1.c*q2.b +q1.d*q2.a)
-- 	return result
-- }
 
-- --Quaternion multiplication without the .a component
-- function quat_pointmult( Quaternion q1, Quaternion q2)
-- {
--         result.x = (q1.a*q2.b +q1.b*q2.a +q1.c*q2.d -q1.d*q2.c)
--         result.y = (q1.a*q2.c -q1.b*q2.d +q1.c*q2.a +q1.d*q2.b)
--         result.z = (q1.a*q2.d +q1.b*q2.c -q1.c*q2.b +q1.d*q2.a)
 
--  return result
-- }

quatRot::Vec3 -> Vec3 -> GLfloat -> Vec3
quatRot point rotVec angle = let sinCoeff = sin (angle * 0.5) :: GLfloat 
                                 cosCoeff = cos (angle * 0.5)::GLfloat
                                 rotQuat = MkQuat (MkVec3 (sinCoeff*(getX rotVec)) (sinCoeff*(getY rotVec)) (sinCoeff*(getZ rotVec))) cosCoeff
                                 pointQuat = MkQuat (MkVec3 (getX point) (getY point) (getZ point)) 0
                                 conjQuat = conjugate rotQuat
                                 temp = quatMul rotQuat pointQuat
                             in 
                                quatPointMul temp conjQuat
                                            
                                            
-- --Uses quaternion mathematics to perform a rotation
-- function quat_rot(Point point,Point rotVec,real angle)
-- {
--         real sinCoeff
--         Quaternion rotQuat
--         Quaternion pointQuat
--         Quaternion conjQuat
--         Quaternion temp
 
--         sinCoeff=sin(angle*0.5)
 
--         rotQuat.a = cos(angle*0.5)
 
--         rotQuat.b=sinCoeff*rotVec.x
--         rotQuat.c=sinCoeff*rotVec.y
--         rotQuat.d=sinCoeff*rotVec.z
 
--         pointQuat.a =0
--         pointQuat.b = point.x
--         pointQuat.c = point.y
--         pointQuat.d = point.z
--         --calculate conjugate of the quaternion
--         conjQuat.a = rotQuat.a
--         conjQuat.b = -rotQuat.b
--         conjQuat.c = -rotQuat.c
--         conjQuat.d = -rotQuat.d
 
--         --perform  rotation
--         temp=quat_mult(rotQuat,pointQuat)
--         point=quat_pointmult(temp,conjQuat)
 
--  return point
-- }

