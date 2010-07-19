import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import Vector
import Quaternion
import Debug.Trace

data State = State { angle :: IORef GLfloat, active :: IORef Bool, x0 :: IORef GLint, y0 :: IORef GLint, dx :: IORef GLint, dy :: IORef GLint}

makeState :: IO State
makeState = do
   a <- newIORef 0
   b <- newIORef False
   c <- newIORef 0
   return $ State { angle = a, active = b, x0 = c, y0 = c, dx = c, dy = c }

initAll :: IO ()
initAll = do
  clearColor $= Color4 0 0 0 0
  shadeModel $= Smooth
  
  lighting $= Enabled
  light (Light 0) $= Enabled
  depthFunc $= Just Less
  
idle :: State -> IdleCallback
idle state = do
  -- changeAngle state
   
  -- s <- get (active state)
  -- if s == True
  --   then  rotatef (0.05) (Vector3 1 0 1)
  --   else rotatef (0.0) (Vector3 1 0 1)
    
  postRedisplay Nothing

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  frustum (-1) 1 (-1) 1 5 25
  matrixMode $= Modelview 0
  loadIdentity
  -- let translatef = translate  :: Vector3 GLfloat -> IO ()    
  translatef (Vector3 0 0 (-15))
  
display :: State -> DisplayCallback
display state = do
  clear [ ColorBuffer, DepthBuffer ]
  
  preservingMatrix $ do
    renderObject Solid (Cube 2.0)
  swapBuffers
    
keyboard :: State -> KeyboardMouseCallback
keyboard _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard state (MouseButton b) Down _ (Position x y) = do
  a <- get (active state)
  active state $= not a
  x0 state $= x
  y0 state $= y
  dx state $= 0
  dy state $= 0
-- (_, Size width height) <- get viewport
-- keyboard state (MouseButton LeftButton) Up _ (Position x y) = do
keyboard _ _           _    _ _ = return ()

to_degrees angle = angle * 57.2957795130823208767981548141052

motion :: State -> MotionCallback
motion state (Position x y) = do
  act <- get (active state)
  if act == True
    then do x0acc <- get (x0 state)
            y0acc <- get (y0 state)
            dx0 <- get (dx state)
            dy0 <- get (dy state)
            (_, Size width height) <- get viewport
            let incr = update (fromIntegral width) (fromIntegral height) (fromIntegral (x - x0acc)) (fromIntegral (y0acc - y)) (fromIntegral x) (fromIntegral y)
                -- 
                incr_vec = (getV incr)
            
            rotatef ((getW (trace( "incr = " ++ show incr) incr)) * 1.0) (Vector3 (getX incr_vec) (getY incr_vec) (getZ incr_vec))
            dx state $= x - x0acc
            dy state $= y0acc - y
            x0 state $= x
            y0 state $= y
            --translatef (Vector3 0 0 (-15))
            -- translatef (Vector3 (0.0 :: GLfloat) ( 0.0 :: GLfloat ) (0.0 :: GLfloat))
            
    else do dx state $= 0
            dy state $= 0
            x0acc <- get (x0 state)
            y0acc <- get (y0 state)
            x0 state $= x
            y0 state $= y
            active state $= not act
            (_, Size width height) <- get viewport
            let incr = update (fromIntegral width) (fromIntegral height) (fromIntegral 0) (fromIntegral 0) (fromIntegral x) (fromIntegral y)
            rotatef 0 (Vector3 0 1 0)
            
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


-- make_quat :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [GLfloat] 
-- make_quat x y z angle = if sq_norm <= 10e-6
--                         then [0.0, 0.0, 0.0, 1.0]
--                         else [sin_theta * x, sin_theta * y, sin_theta * z, cos theta]
--                           where sq_norm = x*x + y*y + z*z
--                                 theta = angle * 0.5
--                                 sin_theta = sin theta
                                
multiply_quat :: [GLfloat] -> [GLfloat] -> [GLfloat]
multiply_quat qr ql = qo
  where
    qr_x = head qr
    qr_y = qr !! 1
    qr_z = qr !! 2
    qr_w = last qr
    ql_x = head ql
    ql_y = ql !! 1
    ql_z = ql !! 2
    ql_w = last ql
    w = ql_w * qr_w - ql_x * qr_x - ql_y * qr_y - ql_z * qr_z
    x = ql_w * qr_x + ql_x * qr_w + ql_y * qr_z - ql_z * qr_y
    y = ql_w * qr_y + ql_y * qr_w + ql_z * qr_x - ql_x * qr_z
    z = ql_w * qr_z + ql_z * qr_w + ql_x * qr_y - ql_y * qr_x
    qo = [x, y, z, w]

translatef = translate  :: Vector3 GLfloat -> IO ()    
rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()


main :: IO ()
main = do
  (prog, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  createWindow prog
  state <- makeState
  initAll
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard state)
  idleCallback $= Just (idle state)
  motionCallback $= Just (motion state)
  mainLoop
  