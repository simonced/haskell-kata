
import Graphics.Gloss

-- data types
type Length = Float
type Angle = Float

-- Point is (Float, Float)
-- angle at 0 means standing up (12 0' clock)
data Stick = Stick Point Length Angle

-- parameters
wWidth = 600 :: Int
wHeight = 600 :: Int

window :: Display
window = InWindow "test!" (wWidth, wHeight) (10, 10)

stickColor = white

-- MAIN
main :: IO ()
main = display window black mytree

-- my tree function
mytree :: Picture
mytree = makeSticks (0, fromIntegral (- wHeight) / 2) 100 0


makeSticks :: Point -> Length -> Angle -> Picture
makeSticks p1 l a = color stickColor $ line points
  where points = [p1, p2]
        p2 = calculatePoint2 p1 l a

calculatePoint2 :: Point -> Length -> Angle -> Point
calculatePoint2 (x, y) l a = (x, y + l)
  -- TODO use the angle
