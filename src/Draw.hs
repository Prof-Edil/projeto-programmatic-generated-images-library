module Draw where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

base_scale :: Float
base_scale = 1000.0

draw :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> IO ()
draw path lines = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ drawLines lines
    writePng path img


drawShapes :: FilePath -> [Drawing PixelRGBA8 ()] -> IO ()
drawShapes path drawings = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ drawings
    writePng path img

drawFill :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> IO ()
drawFill path base_img = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                fillForm FillWinding $ scale 1000 base_img
    writePng path img


drawLines :: (Geometry geom, Transformable geom) => [geom] -> [Drawing px ()]
drawLines img = (\b -> stroke 2 JoinRound (CapRound, CapRound) b) <$> scale 1000 img


fillForm :: (Geometry geom, Transformable geom) => FillMethod -> [geom] -> Drawing px ()
fillForm fillMethod img = fillWithMethod fillMethod img

--joinImgs :: (Geometry geom, Transformable geom) => [[geom]] -> [geom]
joinImgs imgs = mconcat imgs