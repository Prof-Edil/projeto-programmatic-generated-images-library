module TestDrawings where

import Lib
import CreateDrawings
import Shapes
import Draw
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Codec.Picture( PixelRGBA8( .. ), writePng, Image) -- Tirar

-- Modela desenhos para testes


testOut1 = [CubicBezier (V2 1.00 1.00) (V2 0.85 0.95) (V2 0.70 0.95) (V2 0.50 0.98)
           ,CubicBezier (V2 0.50 0.98) (V2 0.30 0.70) (V2 0.15 0.60) (V2 0.00 1.00)
           ]

testFig          = fish
img_base         = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  rot testFig]
img_rot          = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  rot testFig]
img_rot45        = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  rot45 testFig]
img_img2         = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  img2 testFig]
img_img3         = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  img3 testFig]
img_u            = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  u testFig]
img_t            = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  t testFig]
img_v            = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  v testFig]
img_quartet      = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  quartet testFig testFig testFig testFig]
img_side2        = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  side 2 testFig]
img_corner2      = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  corner 2 testFig]
img_squarelimit2 = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  squarelimit 2 testFig]
img_squarelimit3 = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  squarelimit 3 testFig]
img_squarelimit4 = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  squarelimit 4 testFig]

testArc1 = [Line (V2 0.5 0.0) (V2 1.0 0.5)]

seedE :: Int
seedE = -2554405803717694884

colorsE :: [PixelRGBA8]
colorsE = [PixelRGBA8 0xE9 0xE3 0xCE 255, PixelRGBA8 0xFF 0x53 0x73 255, 
          PixelRGBA8 0xEE 0xAD 0x2D 255, PixelRGBA8 0x41 0x69 0xE1 255,
          PixelRGBA8 0x5F 0x4B 0x8B 255, PixelRGBA8 0x4B 0x8B 0x3B 255]

semiCiclesFill = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 2 (randomArcs seedE) semicircle)]
semiCicles     = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ makeQuartets 2 (randomArcs seedE) semicircle]

semiCiclesSquares     = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 2 (randomArcs seedE) semicircle),
  withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ (makeQuartets 2 (randomArcs seedE) square)]

semiCiclesSquaresFill = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 1 (randomArcs seedE) semicircle),
  withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ (makeQuartets 1 (randomArcs seedE) square)]                     
