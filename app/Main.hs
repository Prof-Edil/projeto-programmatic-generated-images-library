module Main where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Control.Monad as M

otherCurve = beside testImage (Lib.flip testImage)
otherFish = beside fish (Lib.flip fish)
otherFish2 = above fish (Lib.flip fish)
twoFish = over fish (Lib.flip fish)

main :: IO ()
main = putStrLn "someFunc"