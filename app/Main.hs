module Main where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Control.Monad as M


main :: IO ()
main = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (stroke 5 JoinRound (CapRound, CapRound)) (scale 1000 (Lib.flip testImage))
    writePng "test2.png" img