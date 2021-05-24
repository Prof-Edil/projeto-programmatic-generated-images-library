module Draw where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng, Image)
import Graphics.Rasterific

base_scale = 1000


saveDrawing :: FilePath -> Int -> PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> IO ()
saveDrawing path imgscale backgroundColor drawings = writePng path $ createDrawing imgscale backgroundColor drawings


createDrawing :: Int -> PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> Image PixelRGBA8
createDrawing imgscale backgroundColor drawings = renderDrawing imgscale imgscale backgroundColor $ mconcat drawings


drawLines :: (Geometry geom, Transformable geom) => Int ->  Float -> [geom] -> [Drawing PixelRGBA8 ()]
drawLines appscale strokeWidth img = (\b -> stroke strokeWidth JoinRound (CapRound, CapRound) b) <$> scale (fromIntegral appscale) img


fillForm :: (Geometry geom, Transformable geom) => FillMethod -> [geom] -> Drawing PixelRGBA8 ()
fillForm fillMethod img = fillWithMethod fillMethod img


--drawAndWrite2 :: (Geometry geom, Transformable geom) => FilePath -> [geom] -> Integer -> IO ()
--drawAndWrite2 path base_img n = do
    --let white = PixelRGBA8 255 255 255 255
        --black = PixelRGBA8 0 0 0 255
        --img = renderDrawing 1200 1200 white $
            --withTexture (uniformTexture black) $ do
                --sequence_ (applyFuncs (zip (colorBlocks n) (blocks n)))
                --mconcat $ (\b -> stroke (35/ (2 ** fromInteger (n))) JoinRound (CapRound, CapRound) b) <$> scale 1200 base_img
    --writePng path img

