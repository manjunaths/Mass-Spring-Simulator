import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import Vector

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
  let rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()
  s <- get (active state)
  if s == True
    then  rotatef (0.05) (Vector3 1 0 1)
    else rotatef (0.0) (Vector3 1 0 1)
    
  postRedisplay Nothing

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  frustum (-1) 1 (-1) 1 5 25
  matrixMode $= Modelview 0
  loadIdentity
  
  let translatef = translate  :: Vector3 GLfloat -> IO ()    
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

motion :: State -> MotionCallback
motion state (Position x y) = do
  act <- get (active state)
  if act == True
    then do x0acc <- get (x0 state)
            y0acc <- get (y0 state)
            dx state $= x - x0acc
            dy state $= y - y0acc
            x0 state $= x
            y0 state $= y
            (_, Size width height) <- get viewport
            update (fromIntegral width) (fromIntegral height)
    else undefined
  where update w h = 
          let small = (min w h) / 2.0
              offset = MkVec3 (w/2.0) (h/2.0) 0
              a = MkVec3 (x-dx) (h - (y+dy)) 0
              b = MkVec3  x (h - y) 0
              a0 = a .-. offset
              b0 = b .-. offset
              a1 = a0 ./. min
              b1 = a0 ./. min

          in
           small * getx offset

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
  