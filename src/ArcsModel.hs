module ArcsModel where

import Lib
import Draw
import CreateDrawings
import Shapes
import Codec.Picture( PixelRGBA8( .. ), writePng, Image)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

----- Usable funcs

genArcs :: FilePath -> Integer -> Int -> IO ()
genArcs path n seed = saveDrawing path scaleSimpleFig white (simpleFig n seed colorsD arc)

genArcsNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> Int -> [PixelRGBA8] -> IO ()
genArcsNew img path n seed colors = saveDrawing path scaleSimpleFig white (simpleFig n seed colors img)

createNewArc :: Float -> Float -> Float -> Float -> [CubicBezier]
createNewArc x1 y1 x2 y2 = [CubicBezier (V2 0.5 0.0) (V2 x1 y1) (V2 x2 y2) (V2 1.0 0.5)]

---- Helpers

blocksDrawing :: Integer -> [PixelRGBA8] -> Int -> [Drawing PixelRGBA8 ()]
blocksDrawing n colors seed = applyFuncs (zip (colorBlocks n colors seed) (blocks n))

scaleSimpleFig :: Int
scaleSimpleFig = 1200

simpleFig :: (Geometry geom, Transformable geom) => Integer -> Int -> [PixelRGBA8] -> [geom] -> [Drawing PixelRGBA8 ()]
simpleFig n seed colors img = [withTexture (uniformTexture black) $ do mconcat $ blocksDrawing n colors seed,
                           withTexture (uniformTexture black) $ do mconcat $ drawLines scaleSimpleFig (35/ (2 ** fromInteger n)) (arcLimit n img seed)]