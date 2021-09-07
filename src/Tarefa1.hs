{-|
Module: Tarefa1
Description: Módulo Haskell contendo funções para gerar um labirinto do jogo PacMan 
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>
Um módulo Haskell contendo funções para gerar um labirinto do jogo PacMan aleatório, de acordo com os requisitos pré-definidos 




= Introdução

A tarefa 1 foi a tarefa na qual implementamos um mecanismo de geração de labirintos válidos para o jogo Pacman. Ou seja, esta tarefa é a construção da base do jogo. 
É esta tarefa que é responsável pela casa dos fantasmas, túneis, por todo o modelo do labirinto e para garantir que qualquer labirinto aleatório que gerarmos será válido conforme as regras.
Além de tratarmos inputs como a largura dos corredores e a altura do labirinto, também há um número inteiro positivo a ser utilizado como semente. Isto nos garante que sempre que darmos uma altura e uma largura específica e uma semente específica, iremos obter o mesmo resultado. Ou seja, sempre que dermos como inputs altura 12, largura 14 e semente 2 iremos obter o mesmo labirinto.



= Objetivos

O nosso objetivo foi com que todos os labirintos gerados fossem válidos. Para que um labirinto seja válido deve cumprir as seguintes regras contidas no enunciado:

*O labirinto é rodeado por peças __Wall__ (exceto túnel);
*A faixa mais central forma um túnel;

    1. Caso a altura seja ímpar, o túnel será formado por 1 corredor;
    2. Caso a altura seja par, o túnel será formado por 2 corredores.

*Todos os corredores têm o mesmo comprimento;
*A “casa dos fantasmas” está localizada no centro do labirinto:

    1. Altura é sempre 3;
    2. Comprimento do labirinto ímpar -> casa tem comprimento 9;
    3. Comprimento do labirinto par -> casa tem comprimento 8;
    4. A área à volta da casa dos fantasmas deve ser gerada com peças Empty.


= Discussão e Conclusão 

Começamos por fazer o modelo da __casa dos fantasmas__ já rodeada por peças __Empty__. A princípio tentamos gerar metade do labirinto, adicionar a casa e gerar a outra metade do labirinto. O problema que encontramos foi que, caso o labirinto tivesse largura maior que a da casa dos fantasmas, não haveriam peças nenhumas ao redor da casa. Seria como se não existisse labirinto nas redondezas.
Portanto, como a casa de fantasmas já rodeada por peças __Empty__ possui altura 5, decidimos selecionar os cinco corredores do meio do labirinto. Desta forma, conseguimos fazer com que as peças ao redor dos corredores pertencentes ao modelo da casa de fantasmas sejam aleatórias.
Após isto, conseguimos selecionar os corredores para os túneis e apenas trocamos a primeira e última peça por __Empty__. 
Para prosseguir, achamos mais prático dividir o labirinto em três: superior, meio e inferior. Na parte superior e inferior, as peças são geradas aleatoriamente com exceção do primeiro e último corredor que contém apenas __Wall__. Também usamos o mesmo raciocínio do túnel para que a primeira e última peça seja __Wall__.
Com isto, também chegamos a conclusão de que seria mais prático dividir o meio do labirinto também em 3: superior, meio e inferior. Isto para que os túneis pudessem ser implementados corretamente.

Em conclusão, esta tarefa foi divertida de fazer, pois foi nosso primeiro contacto com Haskell. Tivemos dificuldades em ter a lógica necessária para realizar esta tarefa no início, porém com o passar do tempo, conseguimos perceber melhor e criar estratégias e as reaproveitar também.
Foi a partir desta tarefa que aprendemos a dividir tudo em problemas menores para que pudéssemos alcançar o resultado final. E ficamos satisfeitas por conseguirmos fazer com que os labirinto fossem válidos no final.


-}



module Tarefa1 where

import System.Random
import Types 


-- | Modelo de uma amostra de labirinto
--
sampleMaze :: Maze
sampleMaze = [
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
                [Empty, Food Little, Food Little, Food Big, Food Little, Food Big, Food Little, Empty],
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            ]


-- | Dado uma seed retorna uma lista de n inteiros gerados aleatoriamente
--
generateRandom :: Int -> Int -> [Int]
generateRandom n seed = let gen = mkStdGen seed 
                        in take n $ randomRs (0,9) gen 


-- | Dado uma seed retorna um inteiro gerado aleatoriamente
--
nrRandom :: Int -> Int
nrRandom seed = head $ generateRandom 1 seed


-- | Converte uma lista em uma lista de listas de tamanho n 
--
subList :: Int -> [a] -> [[a]]
subList _ [] = []
subList n l = take n l: subList n (drop n l)


{- | Converte um número inteiro em uma Peca 

   *3 <=> Comida Grande;
   *0 <= n < 7 <=> Comida Pequena;
   *7 < n <= 9 <=> Parede;
-}

convertPiece:: Int -> Piece
convertPiece p 
   | p == 3 = Food Big
   | p >= 0 && p <7 = Food Little
   | otherwise = Wall

-- | Converte uma lista de inteiros em um Corredor 
--
convertCorridor :: [Int] -> Corridor
convertCorridor [] = []
convertCorridor (x:xs) = convertPiece x : convertCorridor xs


-- | Converte uma lista de lista de inteiros em um Labirinto
--
convertMaze :: [[Int]] -> Maze
convertMaze [] = []
convertMaze (x:xs) = convertCorridor x : convertMaze xs

-- | Gera labirinto aleatório de largura x e altura y
--

generateMaze' :: Int -> Int -> Int -> Maze
generateMaze' x y s =
                 let random_nrs = generateRandom (x*y) s
                 in convertMaze $ subList x random_nrs

-- | Imprime labirinto
--
printFinalMaze :: Maze -> IO ()
printFinalMaze l = do putStrLn (printMaze (l))


{- | Modelo da casa dos fantasmas com espaços vazios em volta

 = Modelo da casa de fantasmas

  == A casa de fantasmas deve cumprir as seguintes regras: 
        *Se o comprimento do labirinto for par = 9 de largura;
        *Se o comprimento do labirinto for ímpar = 8 de largura;
        *Sempre altura = 3;
        *Área a volta da casa apenas com Empty.
        
        -}

homeGhost :: Int -> Maze 
homeGhost l
   |even l = [
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty], 
               [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty], 
               [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
            ]
   |otherwise = [
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty],
               [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
               [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
            ]   


-- | Dado a posição num labirinto, retorna a lista de peças equivalente à posição
--
idPiecePosition :: [[Piece]] -> Int -> [Piece]
idPiecePosition [] _ = [Empty]
idPiecePosition (x:xs) n 
  |n==0 = x 
  |n/=0 = idPiecePosition xs (n-1)

-- | Função que dá o número inteiro correspondente ao meio do labirinto em relação à __altura__
--
middleMaze :: Maze -> Int
middleMaze (x:xs) = div (length (x:xs)) 2 

-- | Função que dá o número inteiro correspondente ao meio do labirinto em relação à __largura__
-- 
middleWidth :: Maze -> Int 
middleWidth (x:xs) = div (length x) 2

-- | Função que dá o número correspondente ao meio do corredor
--
middleCorridor :: Corridor -> Int
middleCorridor (x:xs) = div (length (x:xs)) 2

{- | Seleciona os cinco corredores do meio. Esta função será utilizada na função de troca de corredores

@
(idPiecePosition (x:xs) ((middleMaze (x:xs))
@
*Devolve a lista de peças correspondente ao meio do corredor no qual será a localização da casa de fantasmas com os espaços vazios em volta
-}

fiveCMiddle :: Maze -> [Corridor]
fiveCMiddle (x:xs) = [
                     (idPiecePosition (x:xs) ((middleMaze (x:xs))-3)),
                     (idPiecePosition (x:xs) ((middleMaze (x:xs))-2)),
                     (idPiecePosition (x:xs) ((middleMaze (x:xs))-1)),
                     (idPiecePosition (x:xs) (middleMaze (x:xs))),
                     (idPiecePosition (x:xs) ((middleMaze (x:xs))+1))
                   
               ]

{- | Troca os cinco corredores do meio pelos da casa de fantasmas

== Explicação pormenorizada da função

@
(x:xs)
@
Corredores do meio do labirinto 

@
(y:ys)
@
Corredores da casa de fantasma


@
even (length x)
@
*Foi necessário separar por largura par e ímpar pois a largura da casa de fantasmas depende deste parâmetro

@
beforeHome = (take ((middleCorridor x) - (middleWidth (homeGhost l))) x
@
*Esta devolve a lista das peças antes da casa de fantasmas com o espaço vazio em volta do corredor x
*__middleCorridor__ x devolve o número inteiro correspondente ao meio do corredor x e __middleWidth__ devolve o número inteiro correspondente ao meio em largura dos corredores pertencentes à casa de fantasmas


@
afterHome = drop ((middleCorridor x) + (middleWidth (homeGhost l)+n)) x)
@
*Esta devolve a lista das peças depois da casa de fantasmas com o espaço vazio em volta do corredor x
*__middleCorridor__ x devolve o número inteiro correspondente ao meio do corredor x e __middleWidth__ devolve o número inteiro correspondente ao meio em largura dos corredores pertencentes à casa de fantasmas
*Para largura par, funciona diretamente. Para largura ímpar, é necessário somar 1 ao middleWidth para que não haja peças em falta

@
changeCorridors xs ys 
@
Chama recursivamente a função para os outros corredores
-}

changeCorridors :: [Corridor] -> [Corridor] -> [Corridor]
changeCorridors _ [] = []
changeCorridors [] (y:ys) = (y:ys)
changeCorridors (x:xs) (y:ys) 
   |even (length x) = (beforeHome ++ y ++ afterHome) : changeCorridors xs ys 
   |otherwise = (beforeHome ++ y ++ afterHomeodd) : changeCorridors xs ys
   where l = if even (length (x:xs)) then 10 else 11
         beforeHome = take ((middleCorridor x) - (middleWidth (homeGhost l))) x
         afterHome = drop ((middleCorridor x) + (middleWidth (homeGhost l)+0)) x
         afterHomeodd = drop ((middleCorridor x) + (middleWidth (homeGhost l)+1)) x

-- | Função que seleciona o corredor do meio de um labirinto com altura ímpar
--
middleOddTunnel :: Maze -> Corridor
middleOddTunnel (x:xs) = (idPiecePosition (x:xs) ((middleMaze (x:xs))))

-- | Função que seleciona a lista de corredores do meio de um labirinto com altura par, pois quando a altura é par há dois corredores com túneis
--
middleEvenTunnel :: Maze -> [Corridor]
middleEvenTunnel (x:xs) = [
                     (idPiecePosition (x:xs) (middleMaze (x:xs))),
                     (idPiecePosition (x:xs) ((middleMaze (x:xs))+1))
                 ]

{- | Função que acrescenta os __túneis__ ao Corredor

@
([Empty] ++ init (drop 1 (x:xs)) ++ [Empty])
@
*Esta função troca a primeira e última peça do corredor por __Empty__ para que sejam os túneis.
*init (drop 1 (x:xs)) o drop 1 devolve o corredor sem a primeira peça e, ao chamar a função init, devolve também sem a última peça
-}

tunnelLimit :: Corridor -> Corridor
tunnelLimit (x:xs) =  ([Empty] ++ init (drop 1 (x:xs)) ++ [Empty])

{- | Tornou-se necessário dividir o labirinto do meio em suas partes /superior/, /meio/ e /inferior/ para que assim seja possível acrescentar os túneis no meio do labirinto
-}

-- | Função que seleciona o labirinto __superior__ do labirinto meio
--
middleSupMaze :: Int -> Int -> Int -> Maze
middleSupMaze 0 0 _ = []
middleSupMaze l a seed = [
                               (idPiecePosition (sup) ((middleMaze (sup))-2)),
                               (idPiecePosition (sup) ((middleMaze (sup))-1))
                          ] 
        where sup = mazeMiddle l a seed 

{- | Função que seleciona o labirinto __inferior__ do labirinto meio

Nesta função houve a necessidade de separar para altura par e ímpar. Pois, caso a altura seja ímpar, há um corredor a mais pertencente  
-}

middleInfMaze :: Int -> Int -> Int -> [Corridor] 
middleInfMaze 0 0 _ = []
middleInfMaze l a seed 
   |odd a =  [
                (idPiecePosition (inf) ((middleMaze (inf))+1)),
                (idPiecePosition (inf) ((middleMaze (inf))+2))
         ] 
   |otherwise = [
                (idPiecePosition (inf) ((middleMaze (inf))+2))
         ]
         where inf = mazeMiddle l a seed 


-- | Função que adiciona o/os túnel/túneis ao meio do labirinto 

middleMazeTunnel :: Int -> Int -> Int -> Maze -> [Corridor]
middleMazeTunnel 0 0 _ (_:_) = []
middleMazeTunnel l a seed (x:xs)
   |odd a = [
             (tunnelLimit (middleOddTunnel (x:xs)))
         ]
   |otherwise = [ 
                  (tunnelLimit (head (middleEvenTunnel (x:xs)))),
                  (tunnelLimit (last (middleEvenTunnel (x:xs))))
               ]
      where total =  generateMaze' l a seed 
     
{- | Função que substitui os /primeiro/ e /último/ elementos dos corredores por __Walls__  

@
([Wall] ++ init (drop 1 (x:xs)) ++ [Wall])
@
   *Esta função troca a primeira e última peça do corredor por _Wall__;
   *init (drop 1 (x:xs)) o drop 1 devolve o corredor sem a primeira peça e, ao chamar a função init, devolve também sem a última peça;
   *Depois a função é chamada recursivamente para o resto do labirinto.
-}

wallLimit :: Maze -> Maze 
wallLimit [] = []
wallLimit (x:xs) = ([Wall] ++ init (drop 1 x) ++ [Wall]) : wallLimit xs 

-- | Gera um corredor só com Walls
--
corridorWall :: Int -> Corridor
corridorWall 0 = []
corridorWall x = Wall : corridorWall (x-1) 

-- | Adiciona último corredor ao labirinto
--
addLastCorridor :: Maze -> Corridor -> Maze
addLastCorridor [] c = [c]
addLastCorridor (x:xs) c = x : addLastCorridor xs c 


{- | Agora, tornou-se necessário gerar o labirinto separadamente em /superior/, /meio/ e /inferior/ para uma melhor abordagem em ordem de cumprir os requisitos necessários
-}

{- | Função que gera a parte __superior__ do labirinto

@
wallLimit (corridorWall l : generateMaze' l (div (a-7) 2) seed)
@
*wallLimit para que tenha paredes nos limites
*corridorWall pois o primeiro corredor deve ser só de paredes
*altura = (div (a-7) 2), pois são 5 corredores correspondentes à casa de fantasmas com os espaços vazios e 2 corredores apenas com paredes. Divide-se por 2 para que seja dada a metade superior
-}

supMaze :: Int -> Int -> Int -> Maze 
supMaze l a seed = wallLimit (corridorWall l : generateMaze' l (div (a-7) 2) seed)


{- | Função que gera a parte __inferior__ do labirinto

@
wallLimit (corridorWall l : generateMaze' l (div (a-x) 2) seed)
@
*wallLimit para que tenha paredes nos limites
*corridorWall pois o primeiro corredor deve ser só de paredes
*foi necessário separar para altura par e ímpar, pois caso o labirinto tenha altura par, abaixo da casa de fantasmas deve haver um corredor a mais do que acima, para que a casa fique centralizada
-}

infMaze :: Int -> Int -> Int -> Maze 
infMaze 0 0 _ = []
infMaze l a seed 
   |odd a = wallLimit (addLastCorridor (generateMaze' l (div (a-7) 2) seed) (corridorWall l))
   |otherwise = wallLimit (addLastCorridor (generateMaze' l (div (a-6)2) seed) (corridorWall l))


{- | Função que gera a parte do __meio__ do labirinto, esta onde a __casa dos fantasmas__ está localizada

@
wallLimit (changeCorridors (fiveCMiddle (lab)) (homeGhost l))
@
Há a troca de corredores dos 5 corredores do meio do labirinto pelos da casa de fantasmas com espaços vazios em volta
-}
mazeMiddle :: Int -> Int -> Int -> Maze 
mazeMiddle 0 0 _ = []
mazeMiddle l a seed = wallLimit (changeCorridors (fiveCMiddle (lab)) (homeGhost l))
   where lab = generateMaze' l a seed


{- | Função que gera labirinto do meio completo

== Nesta etapa, é feita a junção dos labirintos /superior/, /meio/ e /inferior/ do labirinto meio para que seja formado o labirinto meio completo

@
middleSupMaze l a seed ++ middleMazeTunnel l a seed (mazeMiddle l a seed) ++ middleInfMaze l a seed
@
-}
completeMiddleMaze :: Int -> Int -> Int -> Maze
completeMiddleMaze 0 0 _ = []
completeMiddleMaze l a seed = middleSupMaze l a seed ++ middleMazeTunnel l a seed (mazeMiddle l a seed) ++ middleInfMaze l a seed

{- | Função que gera o __Labirinto Completo__ conforme as regras 

== Por fim, precisamos unir tudo com a finalidade de gerar o labirinto completo

@
supMaze l a seed ++ completeMiddleMaze l a seed ++ infMaze l a seed
@
-}
generateCompleteMaze :: Int -> Int -> Int -> Maze
generateCompleteMaze l a seed = supMaze l a seed ++ completeMiddleMaze l a seed ++ infMaze l a seed 