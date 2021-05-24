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

---------------------------------------------
