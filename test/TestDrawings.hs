module TestDrawings where

import Lib
import CreateDrawings
import Shapes
import Draw
import Graphics.Rasterific
import Graphics.Rasterific.Texture


testOut2 = over (rot45 (Lib.flip (rot testOut1))) (rot (rot (rot45 (Lib.flip (rot testOut1))))) 
testOut3 = transform (multFst (-1)) ((rot.rot.rot) (Lib.flip testOut1))

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


arcsDrawing = arcLimit 0 arc

blocksDrawing = applyFuncs (zip (colorBlocks 0) (blocks 0))

scale_simpleFig = 1200
simpleFig = [withTexture (uniformTexture black) $ do mconcat $ blocksDrawing,
  withTexture (uniformTexture black) $ do mconcat $ drawLines scale_simpleFig (35/ (2 ** 0)) arcsDrawing]


semiCiclesFill = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 2 randomArcs semicircle)]
semiCicles     = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ makeQuartets 2 randomArcs semicircle]

semiCiclesSquares     = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 2 randomArcs semicircle),
  withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ (makeQuartets 2 randomArcs square)]

semiCiclesSquaresFill = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets 1 randomArcs semicircle),
  withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ (makeQuartets 1 randomArcs square)]                     