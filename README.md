# Programmatic generated images library

## Descrição

Biblioteca para a geração de imagens de forma programática através da utilização de operações e criações de estruturas baseadas em uma imagem geradora inicial, como visualizado no trabalho 'Functional Geometry' feito por Peter Henderson (https://eprints.soton.ac.uk/257577/1/funcgeo2.pdf). 

O objetivo é que a biblioteca permita tanto a geração de imagens a partir de uma imagem base que atenda certos padrões para que o resultado tenha as características esperadas, quanto a possível adapatação e/ou geração de novas imagens que inicialmente não seriam boas canditadas para esse tipo de algoritmo, padrões esses exemplificados também no trabalho 'Functional Geometry'.

## Modelos 

A biblioteca contém os seguintes modelos: SquareLimit (do artigo Functional Geometry), Arcs e Truchet, ambas reproduzindo as artes de [Caleb Ogg ](https://www.instagram.com/iso.hedron)

### SquareLimit

~~~haskell
-- Gera e salva o padrão SquareLimit usando o peixe default
genSquareLimit :: FilePath -> Integer -> IO ()
~~~

~~~haskell
-- Gera e salva o padrão SquareLimit usando uma imagem informada
-- Para melhores resultados, recomenda-se uma imagem gerada por createNewPattern
genSquareLimitNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> IO ()
~~~
~~~haskell
-- Gera uma imagem com as característica desejadas para o SquareLimit
-- Para um melhor resultado, recebe uma forma base que começa no ponto (V2 0.00 1.00) e termina no (V2 1.00 1.00), ou vice-versa.
createNewPattern :: Transformable a => [a] -> [a]
~~~


### Arcs
~~~haskell
-- Gera e salva uma imagem usando o elemento default
genArcs :: FilePath -> Integer -> Int -> IO ()
~~~
~~~haskell
-- Gera e salva uma imagem usando um elemento informado
-- Para melhores resultados deve começar no ponto (0.5, 0.0) e terminar no ponto (1.0, 0.5), ou vice-versa.
genArcsNew :: (Geometry geom, Transformable geom) => [geom] -> FilePath -> Integer -> Int -> [PixelRGBA8] -> IO ()
~~~

~~~haskell
-- Maneira simples de criar uma curva cúbica de Bezier que começa em (0.5, 0.0) e termina em (1.0, 0.5), ou vice-versa.
createNewArc :: Float -> Float -> Float -> Float -> [CubicBezier]
~~~

### Truchet
~~~haskell
-- Gera e salva o padrão SemiCirclesSquares usando a imagem default
genTruchet :: FilePath -> Integer -> Int -> IO ()
~~~
