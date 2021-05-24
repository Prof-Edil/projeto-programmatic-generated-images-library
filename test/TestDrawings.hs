module TestDrawings where

import Lib
import CreateDrawings
import Shapes
import Draw
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Codec.Picture( PixelRGBA8( .. ), writePng, Image) -- Tirar


testOut2 = over (rot45 (Lib.flip (rot testOut1))) (rot (rot (rot45 (Lib.flip (rot testOut1))))) 
testOut3 = transform (multFst (-1)) ((rot.rot.rot) (Lib.flip testOut1))

semiCircles1 = quartet q1 q1 q1 q4
semiCircles2 = quartet q4 q2 q2 q3
semiCircles3 = quartet q2 q3 q1 q1
semiCircles4 = quartet q1 q3 q4 q1
semiCircles  = quartet semiCircles1 semiCircles2 semiCircles3 semiCircles4

dsemiCircles = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ semiCircles]


test2 = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 q1, 
         withTexture (uniformTexture blue) $ do mconcat $ drawLines 1000 2 q2]

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