{-|
Module: Tarefa3
Description: Módulo Haskell que compacta o labirinto 
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>

Um módulo Haskell que compacta o labirinto com o objetivo de codificar o labirinto para como uma sequência de instruções que, quando executadas, produzem o mesmo labirinto. 

=Introdução
A tarefa 3 é responsável por facilitar a leitura de um labirinto. Isto é, considerando um labirinto válido, o objetivo desta tarefa é convertê-lo numa sequência de instruções de modo a deixar o labirinto num formato mais compacto.

=Objetivo 
O nosso objetivo nesta tarefa era criar uma função que conseguisse converter o labirinto de forma a que o número de instruções a dar aos interpretador fosse o mínimo possível.

=Discussão/Conclusão 
Decidimos dividir esta tarefa em partes mais pequenas. Primeiramente decidimos fazer a conversão total dos corredores. Após isto, pegamos no resultado desta converção e retirámos os corredores que apareciam repetidos, ou seja, substituímos estes por Repeat n, onde o n seria o índice da primeira vez que este corredor aparece. Para isto, escolhemos utilizar acumuladores, entre eles uma lista que começa vazia e os corredores vão sendo adicionados nesta. Caso já tenham corredores iguais, estes n são adicionados e inserimos Repeat n conforme explicado anteriormente. O outro acumulador é relacionado com os índices dos corredores no labirinto e começa em 1 pois já contamos que o corredor 0 é sempre logo adicionado à lista vazia justamente por ser o primeiro corredor. Em conclusão, achamos uma tarefa interessante pois nos obrigou a utilizar acumuladores e aprender mais sobre Haskell em si. Além disso, também é importante para uma pessoa analisar a quantidade de peças do labirinto e se há corredores iguais de maneira mais fácil e mais clara, diferentemente do que temos no terminal por serem strings pequenas e muito juntas. E ficamos felizes por termos conseguido concluir. 




-}
module Tarefa3 where 

import Tarefa1
import Types 
import Data.List 


-- | Função que retorna a peça referente a posição dada numa lista de peças
--
idPieceCorridor :: [Piece] -> Int -> Piece
idPieceCorridor [] _ = Empty
idPieceCorridor (x:xs) n 
  |n==0 = x
  |otherwise = idPieceCorridor xs (n-1)

{- | Função que retorna a lista das primeiras peças iguais

@
takeWhile (==x) (x:xs)
@
*Irá a lista de peças até que encontre uma diferente
-}
--
samePiece :: Corridor -> [Piece]
samePiece [] = []
samePiece (x:xs) = (takeWhile (==x) (x:xs))

{- | Função que retorna instruction de uma lista de peças iguais

@
length p
@
*Quantidade desta peça na lista, já que usaremos para listas que só possuem a mesma peça
-}
--
instructPieces :: [Piece] -> [(Int,Piece)]
instructPieces p = [(length p, idPieceCorridor p 0)]

{- | Função que ao receber um corredor, o divide em listas de peças iguais

@
samePiece (x:xs)
@
*Aplica a função samePiece ao corredor para ter uma lista com peças iguais até que apareça uma diferente

@
partitionCorridor (dropWhile (==x) xs)
@
*Aplica recursivamente ao resto da lista, sendo necessário a utilização da dropWhile para que não repita para as peças iniciais que já foram lidas
-}

partitionCorridor :: Corridor -> [[Piece]]
partitionCorridor [] = []
partitionCorridor (x:xs) = samePiece (x:xs) : partitionCorridor (dropWhile (==x) xs)

-- | Função que ao receber uma lista de lista de peças, retorna as instructions
--
readCorridor :: [[Piece]] -> [(Int,Piece)]
readCorridor [] = []
readCorridor (x:xs) = instructPieces x ++ readCorridor xs 


-- | Função que dado um labirinto devolve as instructions
--
readMaze :: Maze -> Instructions
readMaze [] = []
readMaze (x:xs) = Instruct (readCorridor (partitionCorridor x)) : readMaze xs 

  
{- | Função que recebe o labirinto em Instructions da primeira transformação e o devolve de forma com que os corredores repetidos tenham sido trocados pelos Repeats

= Modificação do labirinto em forma de Instructions para que corredores repetidos estejam na forma de __Repeat n__, na qual n é o índice da __primeira__ ocorrência deste corredor no labirinto

 == Explicação breve da função getInstructions: 

@
map snd $ sort $ repeatNth [] l' 1 
@
        *sort para que estejam em ordem de acordo com Int do tuplo (Int,Instruction);
        *map snd para ter apenas Instruction do tuplo (Int,Instruction) recebido da função repeatNth; 
        *repeatNth [] l' 1 - [] para que comece com [] e o primeiro corredor seja adicionado a esta lista vazia. Deste modo, irá percorrer o restante da lista acumulando os resultados
        *acumulador começa em 1, pois o índice começa em 0;
      
@
where l' = zip [0..] l
@ 
*Deste modo temos o labirinto em formato (Int,Instruction) no qual o Int é o índice da Instruction no labirinto;


        -}

getInstructions :: Instructions -> Instructions
getInstructions l = map snd $ sort $ repeatNth [] l' 1 
  where l' = zip [0..] l 

{- | Função auxiliar responsável por trocar elementos repetidos por Repeat n

== Explicação pormenorizada da função

@
elem x (map snd l2) = repeatNth l2 ls (acc+1) 
@
*Se o elemento está em l2 é porque é repetido

@
mapAcc == []  = repeatNth ((i,x):l2) ls (acc+1)
@
*Insere os elementos em l2 quando não são repetidos

@
otherwise = repeatNth (((i,x):l2)++repeats) ls (acc+1)
@
*Insere os elementos quando são repetidos em l2

@
mapAcc  = map (+acc) (elemIndices x (map snd ls))
@
*Aqui percebe-se a necessidade para o acc começar em 1 explicado anteriormente

@
repeats = map (\y -> (y,(Repeat i)) ) mapAcc 
@
*Adiciona Repeat i no qual i equivale à primeira ocorrência no labirinto e y a posição na qual deve estar

-}

repeatNth :: [(Int, Instruction)] -> [(Int, Instruction)] -> Int -> [(Int, Instruction)]
repeatNth l [] _ = l
repeatNth l2 ((i,x):ls) acc 
  | elem x (map snd l2) = repeatNth l2 ls (acc+1) -- se o elemento está em l2 é porque é repetido
  | mapAcc == []        = repeatNth ((i,x):l2) ls (acc+1) -- insere os elementos em l2 quando não são repetidos
  | otherwise           = repeatNth (((i,x):l2)++repeats) ls (acc+1) -- insere os elementos quando são repetidos em l2
  where mapAcc  = map (+acc) (elemIndices x (map snd ls)) -- aqui percebe-se a necessidade para o acc começar em 1
        repeats = map (\y -> (y,(Repeat i)) ) mapAcc -- adiciona Repeat i no qual i equivale à primeira ocorrência no labirinto e y a posição na qual deve estar


-- | Função final para compactar o labirinto
--
compactMaze :: Maze -> Instructions
compactMaze (x:xs) = getInstructions (readMaze (x:xs))