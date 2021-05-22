module Lib where

import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Prelude hiding (flip)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Image = [CubicBezier]


triangle :: [Line]
triangle = [ Line (V2 0.00 0.00) (V2 1.00 1.00)
           , Line (V2 0.00 0.00) (V2 0.00 1.00)
           , Line (V2 0.00 1.00) (V2 1.00 1.00)
           ]

fish :: [CubicBezier]
fish = [ (CubicBezier (V2 0.00 0.00) (V2 0.08 0.02) (V2 0.22 0.18) (V2 0.29 0.28))
       , (CubicBezier (V2 0.29 0.28) (V2 0.30 0.36) (V2 0.29 0.43) (V2 0.30 0.50))
       , (CubicBezier (V2 0.30 0.50) (V2 0.34 0.60) (V2 0.43 0.68) (V2 0.50 0.74))
       , (CubicBezier (V2 0.50 0.74) (V2 0.58 0.79) (V2 0.66 0.78) (V2 0.76 0.80))
       , (CubicBezier (V2 0.76 0.80) (V2 0.82 0.88) (V2 0.94 0.95) (V2 1.00 1.00))
       , (CubicBezier (V2 1.00 1.00) (V2 0.90 0.97) (V2 0.81 0.96) (V2 0.76 0.95))
       , (CubicBezier (V2 0.76 0.95) (V2 0.69 0.96) (V2 0.62 0.96) (V2 0.55 0.96))
       , (CubicBezier (V2 0.55 0.96) (V2 0.49 0.90) (V2 0.40 0.83) (V2 0.35 0.80))
       , (CubicBezier (V2 0.35 0.80) (V2 0.29 0.76) (V2 0.19 0.72) (V2 0.14 0.69))
       , (CubicBezier (V2 0.14 0.69) (V2 0.09 0.65) (V2 (-0.03) 0.57) (V2 (-0.05) 0.28))
       , (CubicBezier (V2 (-0.05) 0.28) (V2 (-0.04) 0.18) (V2 (-0.02) 0.05) (V2 0.00 0.00))
    
       , (CubicBezier (V2 0.10 0.15) (V2 0.14 0.18) (V2 0.18 0.22) (V2 0.18 0.25))
       , (CubicBezier (V2 0.18 0.25) (V2 0.16 0.26) (V2 0.14 0.27) (V2 0.12 0.27))
       , (CubicBezier (V2 0.12 0.27) (V2 0.11 0.23) (V2 0.11 0.19) (V2 0.10 0.15))
    
       , (CubicBezier (V2 0.05 0.18) (V2 0.10 0.20) (V2 0.08 0.26) (V2 0.09 0.30))
       , (CubicBezier (V2 0.09 0.30) (V2 0.07 0.32) (V2 0.06 0.34) (V2 0.04 0.33))
       , (CubicBezier (V2 0.04 0.33) (V2 0.04 0.27) (V2 0.04 0.19) (V2 0.05 0.18))

       , (CubicBezier (V2 0.11 0.30) (V2 0.16 0.44) (V2 0.24 0.61) (V2 0.30 0.66))
       , (CubicBezier (V2 0.30 0.66) (V2 0.41 0.78) (V2 0.62 0.84) (V2 0.80 0.92))

       , (CubicBezier (V2 0.23 0.20) (V2 0.35 0.20) (V2 0.44 0.22) (V2 0.50 0.25))
       , (CubicBezier (V2 0.50 0.25) (V2 0.50 0.33) (V2 0.50 0.41) (V2 0.50 0.49))
       , (CubicBezier (V2 0.50 0.49) (V2 0.46 0.53) (V2 0.42 0.57) (V2 0.38 0.61))

       , (CubicBezier (V2 0.29 0.29) (V2 0.36 0.26) (V2 0.43 0.27) (V2 0.48 0.31))

       , (CubicBezier (V2 0.34 0.39) (V2 0.38 0.34) (V2 0.44 0.36) (V2 0.48 0.37))

       , (CubicBezier (V2 0.34 0.49) (V2 0.38 0.44) (V2 0.41 0.42) (V2 0.48 0.43))

       , (CubicBezier (V2 0.45 0.58) (V2 0.46 0.60) (V2 0.47 0.61) (V2 0.48 0.61))

       , (CubicBezier (V2 0.42 0.61) (V2 0.43 0.64) (V2 0.46 0.68) (V2 0.48 0.67))

       , (CubicBezier (V2 0.25 0.74) (V2 0.17 0.83) (V2 0.08 0.91) (V2 0.00 0.99))
       , (CubicBezier (V2 0.00 0.99) (V2 (-0.08) 0.91) (V2 (-0.17) 0.82) (V2 (-0.25) 0.74))
       , (CubicBezier (V2 (-0.25) 0.74) (V2 (-0.20) 0.63) (V2 (-0.11) 0.53) (V2 (-0.03) 0.43))

       , (CubicBezier (V2 (-0.17) 0.74) (V2 (-0.13) 0.66) (V2 (-0.08) 0.60) (V2 (-0.01) 0.56))

       , (CubicBezier (V2 (-0.12) 0.79) (V2 (-0.07) 0.71) (V2 (-0.02) 0.66) (V2 0.05 0.60))

       , (CubicBezier (V2 (-0.06) 0.86) (V2 (-0.03) 0.77) (V2 0.03 0.72) (V2 0.10 0.66))

       , (CubicBezier (V2 (-0.02) 0.92) (V2 0.02 0.84) (V2 0.09 0.77) (V2 0.16 0.70))
       ]

--FilePath é uma string específica de arquivos e diretórios

--drawAndWrite :: String -> Image -> IO()
drawAndWrite :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> IO ()
drawAndWrite path base_img = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ (\b -> stroke 2 JoinRound (CapRound, CapRound) b) <$> scale 1000 base_img
    writePng path img

--scale :: Transformable a => Float -> a -> a
--scale s = transform (\(V2 x y) -> V2 (x * s) (y * s))

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

--over :: Image -> Image -> Image
over :: [a] -> [a] -> [a]
over = (++)

--aboveScaled :: Float -> Float -> Image -> Image -> Image
aboveScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
aboveScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multSnd (getProp f1 f2)) img1
             trans2 = transform (addSnd (getProp f1 f2).multSnd (getProp f2 f1)) img2

--above :: Image -> Image -> Image
above :: Transformable a => [a] -> [a] -> [a]
above = aboveScaled 1 1

--besideScaled :: Float -> Float -> Image -> Image -> Image
besideScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
besideScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multFst (getProp f1 f2)) img1
             trans2 = transform (addFst (getProp f1 f2).multFst (getProp f2 f1)) img2

--beside :: Image -> Image -> Image
beside :: Transformable a => [a] -> [a] -> [a]
beside = besideScaled 1 1

--quartet :: Image -> Image -> Image -> Image -> Image
quartet :: Transformable a => [a] -> [a] -> [a] -> [a] -> [a]
quartet a b c d = above (beside a b) (beside c d)


-- rotate (x, y) by an angle a (counter-clockwise) = (x2, y2), with
-- x2 = dx + (x - dx) * cos(a) - (dy - y) * sin(a)
-- y2 = dy - ((dy - y) * cos(a)) + ((x - dx) * sin(a))


-- rot (x, y) by 90º 
-- dx = dy = 1/2 
-- x2 = 1/2 - (1/2 - y)*1 = y
-- y2 = 1/2 - (x - 1/2) = 1 - x

--rot ::  Image -> Image
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

--rot45 :: Image -> Image
rot45 :: Transformable a => a -> a
rot45 = transform (\p -> (*0.5) <$> (addFst (sum p).multFst 0 $ p - (swap p)))

--img2 :: Image -> Image
img2 :: Transformable a => a -> a
img2 = flip.rot45

--img3 :: Image -> Image
img3 :: Transformable a => a -> a
img3 = rot.rot.rot.img2

--blank :: Image
blank :: [a]
blank = []

--u :: Image -> Image
u :: Transformable a => [a] -> [a]
u i = over (over image2 (rot image2)) (over (rot $ rot image2) (rot $ rot $ rot image2))
       where image2 = img2 i

--t :: Image -> Image
t :: Transformable a => [a] -> [a]
t i = over i (over (img2 i) (img3 i))


--side :: Integer -> Image -> Image
side :: Transformable a => Integer -> [a] -> [a]
side 0 _ = blank
side n i = quartet (side (n-1) i) (side (n-1) i) (rot (t i)) (t i)

--corner :: Integer -> Image -> Image
corner :: Transformable a => Integer -> [a] -> [a]
corner 0 _ = blank
corner n i = quartet (corner (n-1) i) (side (n-1) i) (rot $ side (n-1) i) (u i)


--nonet :: Image -> Image -> Image ->
--         Image -> Image -> Image ->
--         Image -> Image -> Image -> Image
nonet :: Transformable a => [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> [a]
nonet  p q r
       s t u
       v w x =
              aboveScaled 1 2 (besideScaled 1 2 p (beside q r)) 
              (above (besideScaled 1 2 s (beside t u)) 
              (besideScaled 1 2 v (beside w x)))

--squarelimit :: Integer -> Image -> Image
squarelimit :: Transformable a => Integer -> [a] -> [a]
squarelimit n i = nonet (corner n i) (side n i)             (rot $ rot $ rot $ corner n i)
                  (rot $ side n i)   (u i)                  (rot $ rot $ rot $ side n i)
                  (rot $ corner n i) (rot $ rot $ side n i) (rot $ rot $ corner n i)