import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Control.Monad as M

otherCurve = beside testImage (Lib.flip testImage)
otherFish = beside fish (Lib.flip fish)
otherFish2 = above fish (Lib.flip fish)
twoFish = over fish (Lib.flip fish)
quartetFish = quartet fish fish fish fish
quartetRand = quartet otherCurve otherFish otherFish2 twoFish

main :: IO ()
main = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (stroke 5 JoinRound (CapRound, CapRound)) (scale 1000 fish)
        img2 = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (stroke 5 JoinRound (CapRound, CapRound)) (scale 1000 otherFish2)
    writePng "test.png" img
    writePng "test2.png" img2
