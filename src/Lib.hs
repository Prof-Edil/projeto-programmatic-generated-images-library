module Lib where

import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Image = [CubicBezier]

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

drawAndWrite :: String -> (Image -> Image) -> IO()
drawAndWrite path f = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ (\b -> stroke 5 JoinRound (CapRound, CapRound) b) <$> scale 1000 (f fish)
    writePng path img


-- scale a image by a factor s
-- CubicBezier is Transformable
scale :: Float -> Image -> Image
scale s = transform (\(V2 x y) -> V2 (x * s) (y * s))


-- rotate (x, y) by an angle a (counter-clockwise) = (x2, y2), with
-- x2 = dx + (x - dx) * cos(a) - (dy - y) * sin(a)
-- y2 = dy - ((dy - y) * cos(a)) + ((x - dx) * sin(a))


-- rot (x, y) by 90º 
-- dx = dy = 1/2 
-- x2 = 1/2 - (1/2 - y)*1 = y
-- y2 = 1/2 - (x - 1/2) = 1 - x
rot ::  Image -> Image
rot = transform (\(V2 x y) -> V2 y (1-x))

-- rot (x, y) by 45º 
-- dx = dy = 0
-- r = srqt 2
-- x2 = x* r/2 - (-y * r/2) = r/2 * (x+y)
-- y2 = - ( (-y * r/2)  + (x * r/2) ) r/2 * (y-x)
-- the image must scale by a factor of r, therefore
-- x2 = (x+y)/2
-- y2 = (y-x)/2
rot45 :: Image -> Image
rot45 = transform (\(V2 x y) -> V2 ((x+y)/2) ((y-x)/2))