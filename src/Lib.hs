module Lib where

import Prelude hiding (flip, cycle)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Tile - bloco unitário

-- Dimensionamento
scale :: Transformable a => Float -> a -> a
scale s = transform (fmap (* s))

-- Funções auxiliares para manipulação dos pontos

-- Adiciona um valor ao segundo elemento (para aplicar shift vertical  numa imagem)
addSnd :: Float -> Point -> Point
addSnd a p = p + V2 0 a

-- Adiciona um valor ao primeiro elemento (para aplicar shift horizontal  numa imagem)
addFst :: Float -> Point -> Point
addFst a p = p + V2 a 0

-- Multiplica primeiro elemento do ponto por um valor
multFst :: Float -> Point -> Point
multFst a p = p * V2 a 1

-- Multiplica segundo elemento do ponto por um valor 
multSnd :: Float -> Point -> Point
multSnd a p = p * V2 1 a

-- Troca os valores
swap :: Point -> Point 
swap (V2 x y) = V2 y x

-- Operações base

-- Gira no eixo vertical
flip :: Transformable a => a -> a
flip = transform (addFst 1.multFst (-1))

getProp :: Fractional a => a -> a -> a
getProp f1 f2 = f1/(f1+f2)

-- Sobrepõe duas entradas
over :: [a] -> [a] -> [a]
over = (++)

-- Duas entradas uma acima da outra e dimensiona pela escala dada
aboveScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
aboveScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multSnd (getProp f1 f2)) img1
             trans2 = transform (addSnd (getProp f1 f2).multSnd (getProp f2 f1)) img2

-- Duas entradas uma acima da outra 
above :: Transformable a => [a] -> [a] -> [a]
above = aboveScaled 1 1

-- Duas entradas uma ao lado da outra e dimensiona pela escala dada
besideScaled :: Transformable a => Float -> Float -> [a] -> [a] -> [a]
besideScaled f1 f2 img1 img2 = trans1 `over` trans2
       where trans1 = transform (multFst (getProp f1 f2)) img1
             trans2 = transform (addFst (getProp f1 f2).multFst (getProp f2 f1)) img2

-- Duas entradas uma ao lado da outra 
beside :: Transformable a => [a] -> [a] -> [a]
beside = besideScaled 1 1

-- Função pura para abstrair o número de randomIO
getRandomIO :: Float -> Float
getRandomIO n = 3*n 


-- rotate (x, y) by an angle a (counter-clockwise) = (x2, y2), with
-- x2 = dx + (x - dx) * cos(a) - (dy - y) * sin(a)
-- y2 = dy - ((dy - y) * cos(a)) + ((x - dx) * sin(a))


-- rot (x, y) by 90º 
-- dx = dy = 1/2 
-- x2 = 1/2 - (1/2 - y)*1 = y
-- y2 = 1/2 - (x - 1/2) = 1 - x
rot :: Transformable a => a -> a
rot = transform (addSnd (1).multSnd (-1).swap)

rotn :: Transformable a => Integer -> a -> a
rotn 0 i = i 
rotn n i = rot $ rotn (n-1) i


-- rot (x, y) by 45º 
-- dx = dy = 0
-- r = srqt 2
-- x2 = x* r/2 - (-y * r/2) = r/2 * (x+y)
-- y2 = - ( (-y * r/2)  + (x * r/2) ) r/2 * (y-x)
-- the image must scale by a factor of r, therefore
-- x2 = (x+y)/2
-- y2 = (y-x)/2
rot45 :: Transformable a => a -> a
rot45 = transform (\p -> (*0.5) <$> (addFst (sum p).multFst 0 $ p - (swap p)))
 

blank :: [a]
blank = []
