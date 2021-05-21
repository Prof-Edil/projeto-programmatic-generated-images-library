module Main where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Control.Monad as M

aboveCurve = aboveScaled 0.5 0.5 testImage (Lib.flip testImage)
aboveFish = aboveScaled 0.5 0.5 fish (Lib.flip fish)

main :: IO ()
main = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (stroke 5 JoinRound (CapRound, CapRound)) (scale 1000 aboveCurve)
        img2 = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (stroke 5 JoinRound (CapRound, CapRound)) (scale 1000 aboveFish)
    writePng "test.png" img
    writePng "test2.png" img2