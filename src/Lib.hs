module Lib where

import Prelude hiding (flip, cycle)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

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


getRandomIO :: Float -> Float
getRandomIO n = 3*n 


-- rotate (x, y) by an angle a (counter-clockwise) = (x2, y2), with
-- x2 = dx + (x - dx) * cos(a) - (dy - y) * sin(a)
-- y2 = dy - ((dy - y) * cos(a)) + ((x - dx) * sin(a))


-- rot (x, y) by 90º 
-- dx = dy = 1/2 
-- x2 = 1/2 - (1/2 - y)*1 = y
-- y2 = 1/2 - (x - 1/2) = 1 - x
rot :: Transformable a => a -> a
rot = transform (addSnd (1).multSnd (-1).swap)

rotn :: Transformable a => Integer -> a -> a
rotn 0 i = i 
rotn n i = rot $ rotn (n-1) i


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
 

blank :: [a]
blank = []

--------------------AQUI
-------------------------------------- 
{-|
genArcs :: FilePath -> Integer -> Int -> [PixelRGBA8] -> IO ()
genArcs path n seed colors = drawAndWriteArcs path (arcLimit n arc) n colors seed


genArcsNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> Int -> [PixelRGBA8] -> IO ()
genArcsNew img path n seed colors = drawAndWriteArcs path (arcLimit n img) n colors seed

createNewArc :: Float -> Float -> Float -> Float -> [CubicBezier]
createNewArc x1 y1 x2 y2 = [CubicBezier (V2 0.5 0.0) (V2 x1 y1) (V2 x2 y2) (V2 1.0 0.5)]

drawAndWriteArcs :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> Integer -> [PixelRGBA8] -> Int -> IO ()
drawAndWriteArcs path base_img n colors seed = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1200 1200 white $
            withTexture (uniformTexture black) $ do
                sequence_ (applyFuncs (zip (colorBlocks n colors seed) (blocks n)))
                mconcat $ (\b -> stroke (35/ (2 ** fromInteger (n))) JoinRound (CapRound, CapRound) b) <$> scale 1200 base_img
    writePng path img
-}
----------------------------
