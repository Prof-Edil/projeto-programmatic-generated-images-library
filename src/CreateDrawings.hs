module CreateDrawings where


import Lib
import Shapes
import Draw
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ))

white = PixelRGBA8 255 255 255 255
black = PixelRGBA8 0 0 0 255
blue  = PixelRGBA8 0 0 255 255


testOut2 = over (rot45 (Lib.flip (rot testOut1))) (rot (rot (rot45 (Lib.flip (rot testOut1))))) 
testOut3 = transform (multFst (-1)) ((rot.rot.rot) (Lib.flip testOut1))

semiCircles1 = quartet q1 q1 q1 q4
semiCircles2 = quartet q4 q2 q2 q3
semiCircles3 = quartet q2 q3 q1 q1
semiCircles4 = quartet q1 q3 q4 q1
semiCircles  = quartet semiCircles1 semiCircles2 semiCircles3 semiCircles4
dsemiCircles = [withTexture (uniformTexture black) $ do mconcat $ drawLines $ semiCircles]


test2 = [withTexture (uniformTexture black) $ do mconcat $ drawLines q1, 
         withTexture (uniformTexture blue) $ do mconcat $ drawLines q2]

testFig          = semiCircles
img_base         = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  rot testFig]
img_rot          = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  rot testFig]
img_rot45        = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  rot45 testFig]
img_img2         = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  img2 testFig]
img_img3         = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  img3 testFig]
img_u            = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  u testFig]
img_t            = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  t testFig]
img_quartet      = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  quartet testFig testFig testFig testFig]
img_side2        = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  side 2 testFig]
img_corner2      = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  corner 2 testFig]
img_squarelimit2 = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  squarelimit 2 testFig]
img_squarelimit3 = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  squarelimit 3 testFig]
img_squarelimit4 = [withTexture (uniformTexture black) $ do mconcat $ drawLines $  squarelimit 4 testFig]

