module CreateDrawings where


import Lib
import Prelude hiding (flip, cycle)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ))
import System.Random as SR


---- Colors RGBA8

white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255
blue :: PixelRGBA8
blue  = PixelRGBA8 0 0 255 255
fordwine  = PixelRGBA8 94 18 36 255

colorsD :: [PixelRGBA8]
colorsD = [PixelRGBA8 0xE9 0xE3 0xCE 255, PixelRGBA8 0xFF 0x53 0x73 255, 
          PixelRGBA8 0xEE 0xAD 0x2D 255, PixelRGBA8 0x41 0x69 0xE1 255]

---- Funções geradoras

img2 :: Transformable a => a -> a
img2 = flip.rot45


img3 :: Transformable a => a -> a
img3 = rot.rot.rot.img2

rot3 :: Transformable a => a -> a
rot3 = rot.rot.rot

-- Coloca quatro imagens em um tile
quartet :: Transformable a => [a] -> [a] -> [a] -> [a] -> [a]
quartet a b c d = above (beside a b) (beside c d)

-- Quatro imagens rotacionadas no tile
u :: Transformable a => [a] -> [a]
u i = over (over image2 (rot image2)) (over (rot $ rot image2) (rot $ rot $ rot image2))
       where image2 = img2 i

-- Tres imagens rotacionadas no tile, uma com o dobro do tamanho
t :: Transformable a => [a] -> [a]
t i = over i (over (img2 i) (img3 i))


-- Padrão do lado do SquareLimit
side :: Transformable a => Integer -> [a] -> [a]
side 0 _ = blank
side n i = quartet (side (n-1) i) (side (n-1) i) (rot (t i)) (t i)

-- Padrão do vértice do SquareLimit
corner :: Transformable a => Integer -> [a] -> [a]
corner 0 _ = blank
corner n i = quartet (corner (n-1) i) (side (n-1) i) (rot $ side (n-1) i) (u i)

-- Divide e preenche o tile com 9 imagens com a mesma escala
nonet :: Transformable a => [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> [a]
nonet  p q r
       s t u
       v w x =
              aboveScaled 1 2 (besideScaled 1 2 p (beside q r)) 
              (above (besideScaled 1 2 s (beside t u)) 
              (besideScaled 1 2 v (beside w x)))

-- Gera a SquareLimit
squarelimit :: Transformable a => Integer -> [a] -> [a]
squarelimit n i = nonet (corner n i) (side n i)             (rot $ rot $ rot $ corner n i)
                  (rot $ side n i)   (u i)                  (rot $ rot $ rot $ side n i)
                  (rot $ corner n i) (rot $ rot $ side n i) (rot $ rot $ corner n i)


cycle :: Transformable a => [a] -> [a]
cycle i = quartet (rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ rot $ flip i) (flip i)

u_centered :: Transformable a => [a] -> [a]
u_centered i = quartet (flip i) (rot $ rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ flip i)

v :: Transformable a => [a] -> [a]
v i = cycle $ rot $ t i

---- Funções geradoras do padrão de Arcs

-- Bloco com até 4 arcos ou 4 sub-blocos arcs
arcs :: (Transformable a) => Integer -> [a] -> Int -> Int -> [a]
arcs 0 i p seed = over (rot3 (img (p+10)) ) (over ((rot.rot) (img (p+11))) (over (img (p+12)) (rot (img (p+13)))))
    where img n
           | chance == 0 = []
           | otherwise = i
             where chance = randomArcs seed !! n
arcs n i p seed = quartet (arcs (n-1) i (p+1+fromInteger n) seed) (arcs (n-1) i (p+2+fromInteger n) seed)
                     (arcs (n-1) i (p+3+fromInteger n) seed) (arcs (n-1) i (p+1+fromInteger n) seed)


-- Bloco do limite dos lados da figura            
sideArc :: Transformable a => Integer -> [a] -> Int -> Int -> [a]
sideArc 0 i p seed = quartet (over (img (p+6)) (rot3 (img (p+9)))) (arcs 0 i (p+1) seed) 
                        (over (img (p+7)) (rot3 (img (p+8)))) (arcs 0 i (p-1) seed)
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs seed !! n
sideArc n i p seed= quartet (sideArc (n-1) i (p+3+fromInteger n) seed) (arcs n i (p+2+fromInteger n) seed) 
                        (sideArc (n-1) i (p-1+fromInteger n) seed) (arcs n i (p+fromInteger n) seed)

-- Bloco dos vértices da figura                        
cornerArc :: Transformable a => Integer -> [a] -> Int -> Int -> [a]
cornerArc 0 i p seed = quartet (over (img (p+9)) (rot3 (img (p+8)))) (arcs 0 i (p+5) seed) 
                          (img (p+4)) (rot $ over (img (p+6)) (rot3 (img (p+4))))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs seed !! n
cornerArc n i p seed = quartet (sideArc (n-1) i (p+3+fromInteger n) seed) (arcs n i (p+7+fromInteger n) seed) 
                          (cornerArc (n-1) i (p+6+fromInteger n) seed) (rot $ sideArc (n-1) i (p+3+fromInteger n) seed)

-- Figura completa com os arcos                          
arcLimit :: Transformable a => Integer -> [a] -> Int -> [a]
arcLimit n i seed = nonet (rot3 (cornerArc n i 0 seed)) (rot3 (sideArc n i 1 seed)) ((rot.rot) (cornerArc n i 2 seed)) 
                     (sideArc n i 3 seed) (arcs (n+1) i 4 seed) ((rot.rot) (sideArc n i 5 seed)) 
                     (cornerArc n i 6 seed) (rot (sideArc n i 7 seed)) (rot (cornerArc n i 8 seed))

-- Array infinito de números gerados pseudo-randomicamente dependente da seed, com valores entre 0 e n
randomNumbers :: Int -> Int -> [Int]
randomNumbers n seed = randomRs (0,n) (mkStdGen seed)

-- Caso específico de 0 a 2 de randomNumbers
randomArcs :: Int -> [Int]
randomArcs = randomNumbers 2

-- Aplica funções à valores 2 a 2
applyFuncs :: [(t -> b, t)] -> [b]
applyFuncs = map (\x -> (fst x) (snd x))

---- Funções geradoras dos blocos em Arcs

-- Informa ao mesh o número de blocos total e tamanho de cada um deles
blocks :: Integer -> [[Primitive]]
blocks n = mesh number size
    where number = round ((6*2**fromInteger n)**2)
          size = 1200 / (6*2**fromInteger n)

-- Cria uma grid de quadrados          
mesh :: Integer -> Float -> [[Primitive]]
mesh 0 _ = []
mesh n size = rectangle (V2 (fromInteger x*size) (fromInteger y*size)) size size : mesh (n-1) size
        where x = mod n (round (1200/size))
              y = round ((1200/size) - 1) - div (n-1) (round (1200/size))

-- Informa à coloring o número de blocos no total
colorBlocks :: Geometry geom => Integer -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
colorBlocks n = coloring number
    where number = round ((6*2**(fromInteger n+1))**2)

-- Cria array com funções que colorem os quadrados
coloring :: Geometry geom => Int -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
coloring 0 _ _ = []
coloring n xs seed = withTexture (uniformTexture (xs !! (randomNumbers (length xs - 1) seed !! n))) . fill : coloring (n-1) xs seed


-------------------------------------------
makeQuartets :: Transformable a => Integer -> [Int] -> [a] -> [a]
makeQuartets 0 randoml i = quartet (scale 0.85 $ rotn n1 i) (scale 0.85 $ rotn n2 i) (scale 0.85 $ rotn n3 i) (scale 0.85 $ rotn n4 i) where 
  n1 = toInteger $ randoml !! 0
  n2 = toInteger $ randoml !! 1
  n3 = toInteger $ randoml !! 2
  n4 = toInteger $ randoml !! 3
makeQuartets n randoml i = quartet (makeQuartets (n-1) l1 i) (makeQuartets (n-1) l2 i) (makeQuartets (n-1) l3 i) (makeQuartets (n-1) l4 i) where 
  l1 = drop 4 randoml
  l2 = drop 4 l1
  l3 = drop 4 l2
  l4 = drop 4 l3