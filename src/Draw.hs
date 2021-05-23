module Draw where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng, Image)
import Graphics.Rasterific

base_scale = 1000


saveDrawing :: FilePath -> PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> IO ()
saveDrawing path backgroundColor drawings = writePng path $ createDrawing backgroundColor drawings


createDrawing :: PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> Image PixelRGBA8
createDrawing backgroundColor drawings = renderDrawing base_scale base_scale backgroundColor $ mconcat drawings


drawLines :: (Geometry geom, Transformable geom) => [geom] -> [Drawing PixelRGBA8 ()]
drawLines img = (\b -> stroke 2 JoinRound (CapRound, CapRound) b) <$> scale (fromIntegral base_scale) img


fillForm :: (Geometry geom, Transformable geom) => FillMethod -> [geom] -> Drawing PixelRGBA8 ()
fillForm fillMethod img = fillWithMethod fillMethod img
