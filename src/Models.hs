module Models where

import Lib
import Draw
import CreateDrawings
import Shapes
import Codec.Picture( PixelRGBA8( .. ), writePng, Image)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

----- Usable funcs Arcs

genArcs :: FilePath -> Integer -> Int -> IO ()
genArcs path n seed = saveDrawing path scaleSimpleFig white (simpleFig n seed colorsD arc)

genArcsNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> Int -> [PixelRGBA8] -> IO ()
genArcsNew img path n seed colors = saveDrawing path scaleSimpleFig white (simpleFig n seed colors img)

createNewArc :: Float -> Float -> Float -> Float -> [CubicBezier]
createNewArc x1 y1 x2 y2 = [CubicBezier (V2 0.5 0.0) (V2 x1 y1) (V2 x2 y2) (V2 1.0 0.5)]

---- Helpers Arcs

blocksDrawing :: Integer -> [PixelRGBA8] -> Int -> [Drawing PixelRGBA8 ()]
blocksDrawing n colors seed = applyFuncs (zip (colorBlocks n colors seed) (blocks n))

scaleSimpleFig :: Int
scaleSimpleFig = 1200

simpleFig :: (Geometry geom, Transformable geom) => Integer -> Int -> [PixelRGBA8] -> [geom] -> [Drawing PixelRGBA8 ()]
simpleFig n seed colors img = [withTexture (uniformTexture black) $ do mconcat $ blocksDrawing n colors seed,
                           withTexture (uniformTexture black) $ do mconcat $ drawLines scaleSimpleFig (35/ (2 ** fromInteger n)) (arcLimit n img seed)]

----- Usable funcs Fish

genSquareLimit :: FilePath -> Integer -> IO ()
genSquareLimit path n = saveDrawing path 1000 white (squareLimitFig n fish)

genSquareLimitNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> IO ()
genSquareLimitNew img path n = saveDrawing path 1000 white (squareLimitFig n img)

---- Helper SquareLimit

squareLimitFig :: (Geometry geom, Transformable geom) => Integer -> [geom] -> [Drawing PixelRGBA8 ()]
squareLimitFig n img = [withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $  squarelimit n img]

----- Usable funcs SemiCirclesSquares

genSemiCirclesSquares :: FilePath -> Integer -> Int -> IO ()
genSemiCirclesSquares path n seed = saveDrawing path 1000 white (semiCirclesSquaresFig n seed)

---- Helper SemiCirclesSquares

semiCirclesSquaresFig :: Integer -> Int -> [Drawing PixelRGBA8 ()]
semiCirclesSquaresFig n seed = [withTexture (uniformTexture fordwine) $ do fill $ scale 1000.0 (makeQuartets n (randomArcs seed) semicircle),
                                withTexture (uniformTexture black) $ do mconcat $ drawLines 1000 2 $ (makeQuartets n (randomArcs seed) square)]                     
