module CreateDrawings where


import Lib
import Prelude hiding (flip, cycle)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ))
import System.Random as SR

white = PixelRGBA8 255 255 255 255
black = PixelRGBA8 0 0 0 255
blue  = PixelRGBA8 0 0 255 255

img2 :: Transformable a => a -> a
img2 = flip.rot45


img3 :: Transformable a => a -> a
img3 = rot.rot.rot.img2

rot3 :: Transformable a => a -> a
rot3 = rot.rot.rot

quartet :: Transformable a => [a] -> [a] -> [a] -> [a] -> [a]
quartet a b c d = above (beside a b) (beside c d)

u :: Transformable a => [a] -> [a]
u i = over (over image2 (rot image2)) (over (rot $ rot image2) (rot $ rot $ rot image2))
       where image2 = img2 i


t :: Transformable a => [a] -> [a]
t i = over i (over (img2 i) (img3 i))



side :: Transformable a => Integer -> [a] -> [a]
side 0 _ = blank
side n i = quartet (side (n-1) i) (side (n-1) i) (rot (t i)) (t i)


corner :: Transformable a => Integer -> [a] -> [a]
corner 0 _ = blank
corner n i = quartet (corner (n-1) i) (side (n-1) i) (rot $ side (n-1) i) (u i)


nonet :: Transformable a => [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> 
                            [a] -> [a] -> [a] -> [a]
nonet  p q r
       s t u
       v w x =
              aboveScaled 1 2 (besideScaled 1 2 p (beside q r)) 
              (above (besideScaled 1 2 s (beside t u)) 
              (besideScaled 1 2 v (beside w x)))

squarelimit :: Transformable a => Integer -> [a] -> [a]
squarelimit n i = nonet (corner n i) (side n i)             (rot $ rot $ rot $ corner n i)
                  (rot $ side n i)   (u i)                  (rot $ rot $ rot $ side n i)
                  (rot $ corner n i) (rot $ rot $ side n i) (rot $ rot $ corner n i)


circumference :: Transformable a => [a] -> [a]
circumference i = quartet (rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ rot $ flip i) (flip i)

u_centered :: Transformable a => [a] -> [a]
u_centered i = quartet (flip i) (rot $ rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ flip i)

cycle :: Transformable a => [a] -> [a]
cycle i = over (circumference i)  (u_centered i)

---------------------------------------------
arcs :: (Transformable a) => Integer -> [a] -> Int -> [a]
arcs 0 i p = over (rot3 (img (p+10)) ) (over ((rot.rot) (img (p+11))) (over (img (p+12)) (rot (img (p+13)))))
    where img n
           | chance == 0 = []
           | otherwise = i
             where chance = randomArcs !! n
arcs n i p = quartet (arcs (n-1) i (p+1+fromInteger n)) (arcs (n-1) i (p+2+fromInteger n))
                     (arcs (n-1) i (p+3+fromInteger n)) (arcs (n-1) i (p+1+fromInteger n))

sideArc :: Transformable a => Integer -> [a] -> Int -> [a]
sideArc 0 i p = quartet (over (img (p+6)) (rot3 (img (p+9)))) (arcs 0 i (p+1)) 
                        (over (img (p+7)) (rot3 (img (p+8)))) (arcs 0 i (p-1))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs !! n
sideArc n i p = quartet (sideArc (n-1) i (p+3+fromInteger n)) (arcs n i (p+2+fromInteger n)) 
                        (sideArc (n-1) i (p-1+fromInteger n)) (arcs n i (p+fromInteger n))

cornerArc :: Transformable a => Integer -> [a] -> Int -> [a]
cornerArc 0 i p = quartet (over (img (p+9)) (rot3 (img (p+8)))) (arcs 0 i (p+5)) 
                          (img (p+4)) (rot $ over (img (p+6)) (rot3 (img (p+4))))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs !! n
cornerArc n i p = quartet (sideArc (n-1) i (p+3+fromInteger n)) (arcs n i (p+7+fromInteger n)) 
                          (cornerArc (n-1) i (p+6+fromInteger n)) (rot $ sideArc (n-1) i (p+3+fromInteger n))

arcLimit :: Transformable a => Integer -> [a] -> [a]
arcLimit n i = nonet (rot3 (cornerArc n i 0)) (rot3 (sideArc n i 1)) ((rot.rot) (cornerArc n i 2)) 
                     (sideArc n i 3) (arcs (n+1) i 4) ((rot.rot) (sideArc n i 5)) 
                     (cornerArc n i 6) (rot (sideArc n i 7)) (rot (cornerArc n i 8))

randomArcs :: [Int]
randomArcs = randomRs (0,2) (mkStdGen seed)


randomNumbers :: [Int]
randomNumbers = randomRs (0,3) (mkStdGen seed)

applyFuncs = map (\x -> (fst x) (snd x))

seed :: Int
seed = -2554405803717694884

---------------------------------------------

blocks n = mesh number size
    where number = round ((6*2**fromInteger n)**2)
          size = 1200 / (6*2**fromInteger n)

mesh 0 _ = []
mesh n size = rectangle (V2 (fromInteger x*size) (fromInteger y*size)) size size : mesh (n-1) size
        where x = mod n (round (1200/size))
              y = round ((1200/size) - 1) - div (n-1) (round (1200/size))

colorBlocks n = coloring number
    where number = round ((6*2**(fromInteger n+1))**2)

coloring 0 = []
coloring n = withTexture (uniformTexture (colors !! (randomNumbers !! n))) . fill : coloring (n-1)

colors :: [PixelRGBA8]
colors = [PixelRGBA8 0xE9 0xE3 0xCE 255, PixelRGBA8 0xFF 0x53 0x73 255, 
          PixelRGBA8 0xEE 0xAD 0x2D 255, PixelRGBA8 0x41 0x69 0xE1 255]


-------------------------------------------


