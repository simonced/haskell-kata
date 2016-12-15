-- launch in ghci with -fno-ghci-sandbox

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle -- form radToDeg and degToRad

-- data types
type Length = Float
type Angle = Float -- in degrees

-- Point is (Float, Float)
-- angle at 0 means standing up (12 0' clock)
data Stick = Stick Point Length Angle

-- parameters
wWidth = 600 :: Int
wHeight = 600 :: Int

window :: Display
window = InWindow "test!" (wWidth, wHeight) (10, 10)

stickColor = orange
stickSubAngleLeft = pi/7
stickSubAngleRight = pi/9
stickSubLengthRatio = 0.8
stickLengthMin = 10
leafColor = green

-- MAIN
main :: IO ()
main = display window black mytree

-- my tree function
mytree :: Picture
--mytree = makeSticks (0, fromIntegral (- wHeight) / 2) 100 (pi/6)
mytree = pictures $ makeSticks (0, fromIntegral (- wHeight) / 2) 100 0.0


makeSticks :: Point -> Length -> Angle -> [Picture]
makeSticks p1 l a = stick : children
  where 
        children = if lastStick then [] else child1 ++ child2
        child1 = makeSticks p2 (l*stickSubLengthRatio) (a + stickSubAngleLeft)
        child2 = makeSticks p2 (l*stickSubLengthRatio) (a - stickSubAngleRight)
        stick = color getStickColor $ line points
        points = [p1, p2]
        p2 = calculatePoint2 p1 l a
        lastStick = if l < stickLengthMin then True else False 
        getStickColor = if lastStick then leafColor else stickColor


calculatePoint2 :: Point -> Length -> Angle -> Point
calculatePoint2 (x, y) l a = (x2, y2)
    where x2 = l * sin a + x
          y2 = l * cos a + y
