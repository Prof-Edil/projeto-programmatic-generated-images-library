module CreateDrawings where


import Lib
import Prelude hiding (flip, cycle)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ))
import System.Random as SR

white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255
blue :: PixelRGBA8
blue  = PixelRGBA8 0 0 255 255

colorsE :: [PixelRGBA8]
colorsE = [PixelRGBA8 0xE9 0xE3 0xCE 255, PixelRGBA8 0xFF 0x53 0x73 255, 
          PixelRGBA8 0xEE 0xAD 0x2D 255, PixelRGBA8 0x41 0x69 0xE1 255]

----------------------------------------

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


cycle :: Transformable a => [a] -> [a]
cycle i = quartet (rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ rot $ flip i) (flip i)

u_centered :: Transformable a => [a] -> [a]
u_centered i = quartet (flip i) (rot $ rot $ rot $ flip i) (rot $ flip i) (rot $ rot $ flip i)

v :: Transformable a => [a] -> [a]
v i = cycle $ rot $ t i

---------------------------------------------

arcs :: (Transformable a) => Integer -> [a] -> Int -> Int -> [a]
arcs 0 i p seed = over (rot3 (img (p+10)) ) (over ((rot.rot) (img (p+11))) (over (img (p+12)) (rot (img (p+13)))))
    where img n
           | chance == 0 = []
           | otherwise = i
             where chance = randomArcs seed !! n
arcs n i p seed = quartet (arcs (n-1) i (p+1+fromInteger n) seed) (arcs (n-1) i (p+2+fromInteger n) seed)
                     (arcs (n-1) i (p+3+fromInteger n) seed) (arcs (n-1) i (p+1+fromInteger n) seed)

sideArc :: Transformable a => Integer -> [a] -> Int -> Int -> [a]
sideArc 0 i p seed = quartet (over (img (p+6)) (rot3 (img (p+9)))) (arcs 0 i (p+1) seed) 
                        (over (img (p+7)) (rot3 (img (p+8)))) (arcs 0 i (p-1) seed)
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs seed !! n
sideArc n i p seed= quartet (sideArc (n-1) i (p+3+fromInteger n) seed) (arcs n i (p+2+fromInteger n) seed) 
                        (sideArc (n-1) i (p-1+fromInteger n) seed) (arcs n i (p+fromInteger n) seed)

cornerArc :: Transformable a => Integer -> [a] -> Int -> Int -> [a]
cornerArc 0 i p seed = quartet (over (img (p+9)) (rot3 (img (p+8)))) (arcs 0 i (p+5) seed) 
                          (img (p+4)) (rot $ over (img (p+6)) (rot3 (img (p+4))))
    where img n
            | chance == 0 = []
            | otherwise = i
                where chance = randomArcs seed !! n
cornerArc n i p seed = quartet (sideArc (n-1) i (p+3+fromInteger n) seed) (arcs n i (p+7+fromInteger n) seed) 
                          (cornerArc (n-1) i (p+6+fromInteger n) seed) (rot $ sideArc (n-1) i (p+3+fromInteger n) seed)

arcLimit :: Transformable a => Integer -> [a] -> Int -> [a]
arcLimit n i seed = nonet (rot3 (cornerArc n i 0 seed)) (rot3 (sideArc n i 1 seed)) ((rot.rot) (cornerArc n i 2 seed)) 
                     (sideArc n i 3 seed) (arcs (n+1) i 4 seed) ((rot.rot) (sideArc n i 5 seed)) 
                     (cornerArc n i 6 seed) (rot (sideArc n i 7 seed)) (rot (cornerArc n i 8 seed))


randomNumbers :: Int -> Int -> [Int]
randomNumbers n seed = randomRs (0,n) (mkStdGen seed)


randomArcs :: Int -> [Int]
randomArcs = randomNumbers 2

applyFuncs = map (\x -> (fst x) (snd x))

---------------------------------------------

blocks n = mesh number size
    where number = round ((6*2**fromInteger n)**2)
          size = 1200 / (6*2**fromInteger n)

mesh 0 _ = []
mesh n size = rectangle (V2 (fromInteger x*size) (fromInteger y*size)) size size : mesh (n-1) size
        where x = mod n (round (1200/size))
              y = round ((1200/size) - 1) - div (n-1) (round (1200/size))

colorBlocks :: Geometry geom => Integer -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
colorBlocks n = coloring number
    where number = round ((6*2**(fromInteger n+1))**2)

coloring :: Geometry geom => Int -> [PixelRGBA8] -> Int -> [geom -> Drawing PixelRGBA8 ()]
coloring 0 _ _ = []
coloring n xs seed = withTexture (uniformTexture (xs !! (randomNumbers 3 seed !! n))) . fill : coloring (n-1) xs seed


-------------------------------------------


