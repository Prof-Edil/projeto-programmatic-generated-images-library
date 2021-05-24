module Lib where

import Prelude hiding (flip, cycle)
import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Random as SR
import Prelude hiding (flip)
import Shapes


someFunc :: IO ()
someFunc = putStrLn "someFunc"



drawAndWrite :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> IO ()
drawAndWrite path base_img = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ (\b -> stroke 2 JoinRound (CapRound, CapRound) b) <$> scale 1000 base_img
    writePng path img


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

-------------------------------------- 

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

applyFuncs = map (\x -> (fst x) (snd x))

rot3 :: Transformable a => a -> a
rot3 = rot.rot.rot

arcs :: (Transformable a) => Integer -> [a] -> Int -> [a]
arcs 0 i p = over (rot3 (img (p+10)) ) (over ((rot.rot) (img (p+11))) (over (img (p+12)) (rot (img (p+13)))))
    where img n
           | chance == 0 = []
           | otherwise = i
             where chance = randomArcs !! n
arcs n i p = quartet (arcs (n-1) i (p+1+fromInteger n)) (arcs (n-1) i (p+2+fromInteger n))
                     (arcs (n-1) i (p+3+fromInteger n)) (arcs (n-1) i (p+1+fromInteger n))

sideArc :: Transformable a => Integer -> [a] -> Int -> [a]
sideArc 0 i p = quartet (over (img (p+6)) (rot3 (img (p+9)))) (arcs 0 i (p+1)) 
                        (over (img (p+7)) (rot3 (img (p+8)))) (arcs 0 i (p-1))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs !! n
sideArc n i p = quartet (sideArc (n-1) i (p+3+fromInteger n)) (arcs n i (p+2+fromInteger n)) 
                        (sideArc (n-1) i (p-1+fromInteger n)) (arcs n i (p+fromInteger n))

cornerArc :: Transformable a => Integer -> [a] -> Int -> [a]
cornerArc 0 i p = quartet (over (img (p+9)) (rot3 (img (p+8)))) (arcs 0 i (p+5)) 
                          (img (p+4)) (rot $ over (img (p+6)) (rot3 (img (p+4))))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs !! n
cornerArc n i p = quartet (sideArc (n-1) i (p+3+fromInteger n)) (arcs n i (p+7+fromInteger n)) 
                          (cornerArc (n-1) i (p+6+fromInteger n)) (rot $ sideArc (n-1) i (p+3+fromInteger n))

arcLimit :: Transformable a => Integer -> [a] -> [a]
arcLimit n i = nonet (rot3 (cornerArc n i 0)) (rot3 (sideArc n i 1)) ((rot.rot) (cornerArc n i 2)) 
                     (sideArc n i 3) (arcs (n+1) i 4) ((rot.rot) (sideArc n i 5)) 
                     (cornerArc n i 6) (rot (sideArc n i 7)) (rot (cornerArc n i 8))

randomArcs :: [Int]
randomArcs = randomRs (0,2) (mkStdGen seed)
---------------------------------------------

blocks n format = mesh number size
    where number = round ((6*2**fromInteger n)**2)
          size = 1200 / (6*2**fromInteger n)

mesh 0 _ = []
mesh n size = rectangle (V2 (fromInteger x*size) (fromInteger y*size)) size size : mesh (n-1) size
        where x = mod n (round (1200/size))
              y = round ((1200/size) - 1) - div (n-1) (round (1200/size))

colorBlocks :: Geometry geom => Integer -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
colorBlocks n = coloring number
    where number = round ((6*2**(fromInteger n+1))**2)

coloring :: Geometry geom => Int -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
coloring 0 _ _ = []
coloring n xs seed = withTexture (uniformTexture (xs !! (randomNumbers 3 seed !! n))) . fill : coloring (n-1) xs seed

colorsE :: [PixelRGBA8]
colorsE = [PixelRGBA8 0xE9 0xE3 0xCE 255, PixelRGBA8 0xFF 0x53 0x73 255, 
          PixelRGBA8 0xEE 0xAD 0x2D 255, PixelRGBA8 0x41 0x69 0xE1 255]

seed :: Int
seed = -2554405803717694884

--Int > 0
randomNumbers :: Int -> Int -> [Int]
randomNumbers n seed = randomRs (0,n) (mkStdGen seed)

circumference :: Transformable a => [a] -> [a]
circumference i = quartet (rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ rot $ flip i) (flip i)

--u_centered :: Transformable a => [a] -> [a]
--u_centered i = quartet (flip i) (rot $ rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ flip i)

--cycle :: Transformable a => [a] -> [a]
--cycle i = over (circumference i)  (u_centered i)
