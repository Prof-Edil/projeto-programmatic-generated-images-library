module Draw where

import Lib
import Codec.Picture( PixelRGBA8( .. ), writePng, Image)
import Graphics.Rasterific

base_scale = 1000

-- Cria e salva os desenhos contidos numa lista dada e com cor de fundo dada
saveDrawing :: FilePath -> Int -> PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> IO ()
saveDrawing path imgscale backgroundColor drawings = writePng path $ createDrawing imgscale backgroundColor drawings

-- Renderiza os desenhos dados numa lista e retona uma imagem
createDrawing :: Int -> PixelRGBA8 -> [Drawing PixelRGBA8 ()] -> Image PixelRGBA8
createDrawing imgscale backgroundColor drawings = renderDrawing imgscale imgscale backgroundColor $ mconcat drawings

-- Desenha linhas dadas numa lista, formas ou comandos de caminhos (Paths), numa grossura de linha dado como parâmetro e dimensiona o desenho.
drawLines :: (Geometry geom, Transformable geom) => Int ->  Float -> [geom] -> [Drawing PixelRGBA8 ()]
drawLines appscale strokeWidth img = (\b -> stroke strokeWidth JoinRound (CapRound, CapRound) b) <$> scale (fromIntegral appscale) img

-- Cria uma função para posterior preenchimentos das formas com uma cor
fillForm :: (Geometry geom, Transformable geom) => FillMethod -> [geom] -> Drawing PixelRGBA8 ()
fillForm fillMethod img = fillWithMethod fillMethod img

---------------------------------------------
