module Lib where

import Prelude hiding (flip, cycle)
import Graphics.Rasterific

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Tile - bloco unitário

scale :: Transformable a => Float -> a -> a
scale s = transform (fmap (* s))

-- Points auxiliary functions
addSnd :: Float -> Point -> Point
addSnd a p = p + V2 0 a

addFst :: Float -> Point -> Point
addFst a p = p + V2 a 0

multFst :: Float -> Point -> Point
multFst a p = p * V2 a 1

multSnd :: Float -> Point -> Point
multSnd a p = p * V2 1 a

swap :: Point -> Point 
swap (V2 x y) = V2 y x

-- Base operations

flip :: Transformable a => a -> a
flip = transform (addFst 1.multFst (-1))

getProp :: Fractional a => a -> a -> a
getProp f1 f2 = f1/(f1+f2)


over :: [a] -> [a] -> [a]
over = (++)


aboveScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
aboveScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multSnd (getProp f1 f2)) img1
             trans2 = transform (addSnd (getProp f1 f2).multSnd (getProp f2 f1)) img2


above :: Transformable a => [a] -> [a] -> [a]
above = aboveScaled 1 1


besideScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
besideScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multFst (getProp f1 f2)) img1
             trans2 = transform (addFst (getProp f1 f2).multFst (getProp f2 f1)) img2


beside :: Transformable a => [a] -> [a] -> [a]
beside = besideScaled 1 1


quartet :: Transformable a => [a] -> [a] -> [a] -> [a] -> [a]
quartet a b c d = above (beside a b) (beside c d)


-- rotate (x, y) by an angle a (counter-clockwise) = (x2, y2), with
-- x2 = dx + (x - dx) * cos(a) - (dy - y) * sin(a)
-- y2 = dy - ((dy - y) * cos(a)) + ((x - dx) * sin(a))


-- rot (x, y) by 90º 
-- dx = dy = 1/2 
-- x2 = 1/2 - (1/2 - y)*1 = y
-- y2 = 1/2 - (x - 1/2) = 1 - x
rot :: Transformable a => a -> a
rot = transform (addSnd (1).multSnd (-1).swap)



-- rot (x, y) by 45º 
-- dx = dy = 0
-- r = srqt 2
-- x2 = x* r/2 - (-y * r/2) = r/2 * (x+y)
-- y2 = - ( (-y * r/2)  + (x * r/2) ) r/2 * (y-x)
-- the image must scale by a factor of r, therefore
-- x2 = (x+y)/2
-- y2 = (y-x)/2
rot45 :: Transformable a => a -> a
rot45 = transform (\p -> (*0.5) <$> (addFst (sum p).multFst 0 $ p - (swap p)))
 

img2 :: Transformable a => a -> a
img2 = flip.rot45


img3 :: Transformable a => a -> a
img3 = rot.rot.rot.img2


blank :: [a]
blank = []


u :: Transformable a => [a] -> [a]
u i = over (over image2 (rot image2)) (over (rot $ rot image2) (rot $ rot $ rot image2))
       where image2 = img2 i


t :: Transformable a => [a] -> [a]
t i = over i (over (img2 i) (img3 i))



side :: Transformable a => Integer -> [a] -> [a]
side 0 _ = blank
side n i = quartet (side (n-1) i) (side (n-1) i) (rot (t i)) (t i)


corner :: Transformable a => Integer -> [a] -> [a]
corner 0 _ = blank
corner n i = quartet (corner (n-1) i) (side (n-1) i) (rot $ side (n-1) i) (u i)


nonet :: Transformable a => [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> [a]
nonet  p q r
       s t u
       v w x =
              aboveScaled 1 2 (besideScaled 1 2 p (beside q r)) 
              (above (besideScaled 1 2 s (beside t u)) 
              (besideScaled 1 2 v (beside w x)))

squarelimit :: Transformable a => Integer -> [a] -> [a]
squarelimit n i = nonet (corner n i) (side n i)             (rot $ rot $ rot $ corner n i)
                  (rot $ side n i)   (u i)                  (rot $ rot $ rot $ side n i)
                  (rot $ corner n i) (rot $ rot $ side n i) (rot $ rot $ corner n i)


circumference :: Transformable a => [a] -> [a]
circumference i = quartet (rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ rot $ flip i) (flip i)

--u_centered :: Transformable a => [a] -> [a]
--u_centered i = quartet (flip i) (rot $ rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ flip i)

--cycle :: Transformable a => [a] -> [a]
--cycle i = over (circumference i)  (u_centered i)
