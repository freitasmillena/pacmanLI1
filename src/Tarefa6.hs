{-|
Module: Tarefa6
Description: Módulo Haskell que implementa um robô 
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>

Um módulo Haskell que impleta um robô que jogue Pacman automaticamente


=Introdução

A tarefa 6 foi a tarefa na qual implementamos um bot para efetuar jogadas automaticamente com o Pacman. Para que desta forma, possa procurar ganhar o jogo.




=Objetivos

O nosso objetivo principal foi fazer com que de acordo com o estado atual do Pacman no jogo, pudesse seguir estratégias para tornar-se __Mega__ e ganhar pontos ao comer fantasmas ou simplesmente fugir dos fantasmas para que não perdesse vidas e, consequentemente, o jogo.


=Discussão e conclusão 

Começamos por pensar que, caso o Pacman estivesse em modo __Normal__ ou __Dying__ que sua prioridade seria alcançar uma __Food Big__ para que pudesse se tornar Mega e, assim, recuperar-se no jogo. Para isto, fizemos com que a cada jogada obtivéssemos a lista das coordenadas das comidas grandes remanescentes no labirinto. Em seguida, utilizamos a mesma estratégia utilizada na Tarefa 5. Selecionamos a coordenada da comida grande mais próxima da coordenada atual do Pacman e, em seguida, observamos as coordenadas possíveis paraa o Pacman e que não tivessem __Wall__ e optaríamos pela que fosse necessário o menor número de jogadas o possível para alcançá-la.

Chegamos num impasse no qual há a possibilidade de que não exista mais comidas grandes no labirinto. Com isto, decidimos optar por fazer com que o Pacman fuja dos fantasmas. Para isto, a estratégia utilizada foi procurar qual fantasma encontra-se mais próximo da coordenada atual do Pacman e fazer com que o Pacman siga para uma coordenada que precise do maior número de jogadas o possível para o fantasma a alcançar. Também utilizamos estratégias e funções similares da Tarefa 5.

E, finalmente, caso o Pacman estivesse __Mega__, optamos procurar o fantasma mais próximo do Pacman, ou seja que é necessário menor número de jogadas com o Pacman para alcançá-lo, e fazer com que o persiga. Para o perseguir, utilizamos estratégias e funções também utilizadas na Tarefa 5.

O facto desta tarefa ter sido feita após a tarefa 5 foi ótimo, pois pudemos desenvolver raciocínios na 5 já pensando que poderiam ser reaplicados na 6. Esta talvez foi a mais desafiadora, pois a ideia de criar um bot já é por si só tentadora e, ao conseguir estabelecer estratégias para diferentes possibilidades e fazer com que o bot as execute é fascinante. Em conclusão, esta tarefa foi divertida pois tivemos que analisar as possibilidades levando em consideração que nossos fantasmas também procuram estratégias e ficamos felizes com o que conseguimos.

-}


module Tarefa6 where 

import Types
import Tarefa2
import Tarefa5



{- | Função que dado o id e o estado do jogo, devolve a jogada necessária de modo que o bot consiga o melhor desempenho

 = Bot Pacman

   ==/Pacman Mega/

@
Just (Move x (chaseGhosts pac st))
@
*Caso o Pacman esteja mega, seu objetivo é alcançar o fantasma mais próximo de onde está atualmente. Para isto, aplica-se a função __chaseGhosts__ ao estado do jogo recebido 

   ==/Pacman diferente de Mega/
Neste caso, o objetivo do Pacman é consumir __Food Big__ a fim de tornar-se Mega e perseguir fantasmas. Porém, pode haver um momento no qual não há mais __Food Big__. 
Tornou-se necessário separar em duas condições:

       === Quando não há mais __Food Big__ restantes no labirinto 
Quando não há mais, a função coordsFoodBig retorna uma lista vazia, com isto, o Pacman muda sua estratégia de ir até a __Food Big__ para tornar-se Mega e precisa fugir do fantasma que encontra-se mais próximo.

@
Just (Move x (getOut pac st))
@
*Logo, a __Orientation__ utilizada na Play é recebida através da função getOut 

       === Quando há __Food Big__ restantes no labirinto
@
Just (Move x (goToFood pac m))
@
*Ou seja, a __Orientation__ utilizada na Play é recebida através da função goToFood 
-}
bot :: Int -> State -> Maybe Play
bot x (State [] p l)   = Nothing
bot x (State m [] l)   = Nothing
bot x st@(State m p l) = case pacMode pac of 
                                        Mega -> Just (Move x (chaseGhosts pac st))
                                        _    -> case coordsFoodBig m of 
                                                   [] -> Just (Move x (getOut pac st)) 
                                                   _  -> Just (Move x (goToFood pac m))
    where pac = findPlayer x p


{- | Função que dado um labirinto, devolve a lista com as coordenadas das comidas grandes 

@
if (maze !! y) !! x == Food Big then acc2 ++ [(y,x)] else acc2
@
*(maze !! y) retorna o corredor e ao fazer !! x retorna a peça neste corredor 
*Caso retorne Food Big, há a junção do acc2 que começa como lista vazia ++ [(y,x)] que é a coordenada desta Food Big encontrada
*Caso não retorne Food Big, retorna apenas o acc2 que de início é uma lista vazia, mas após um tempo será a lista atual com as coordenadas até então encontradas que possuem Food Big

@
[] [0..(length (head maze) - 1)]
@
*A lista vazia é o acc2 inicial
*A lista inicial pela qual o foldl irá percorrer é [0..(length (head maze) - 1)], na qual (length (head maze) - 1) devolve o índice da última peça do corredor

@
foldl (\acc y -> acc ++ (foldl (\acc2 x -> if (maze !! y) !! x == Food Big then acc2 ++ [(y,x)] else acc2)) [] [0..(length (head maze) - 1)]) [] [0..(length maze - 1)]
@
*Este foldl faz a junção do acc inicial que neste caso é a lista vazia com o resultado do foldl explicado acima
*A lista que percorre é [0..(length maze - 1)] na qual (length maze - 1) devolve o índice do último corredor do labirinto

-}
coordsFoodBig :: Maze -> [Coords]
coordsFoodBig maze = foldl (\acc y -> acc ++ (foldl (\acc2 x -> if (maze !! y) !! x == Food Big then acc2 ++ [(y,x)] else acc2)) [] [0..(length (head maze) - 1)]) [] [0..(length maze - 1)]




{- | Função que recebe o pacman e o labirinto e, com isto, seleciona a coordenada da __Food Big__ mais próxima e retorna a orientação que deve ser utilizada na próxima jogada para que o pacman vá até esta coordenada
@
poss     = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
@
*Estas são as jogadas existentes para o Pacman: D, U, R, L respetivamente. 

@
lp       = notWall poss (x:xs)
@
*Nesta etapa, são selecionadas apenas as coordenadas que não contém __Wall__ das coordenadas relativas às jogadas existentes

@
lc       = coordsFoodBig (x:xs) 
@
*Estas é a lista de coordenadas das __Food Big__ no labirinto 

@
coordBig = bestCoord lc (a,b)
@
*Nesta etapa, tornou-se necessário selecionar a coordenada da __Food Big__ mais próxima ao Pacman utilizando a função presente na Tarefa 5 

@
best     = bestCoord lp coordBig
@
*Com isto, comparamos as coordenadas que não contém __Wall__, ou seja, as que o Pacman realmente pode ir com a coordenada da __Food Big__ mais próxima a fim de selecionar qual coordenada possível do Pacman que o levará para mais próximo da __Food Big__

@
whichOrientation (a,b) best
@
*Por fim, comparamos a coordenada na qual o Pacman se encontra, com a coordenada que ele quer ir na próxima jogada a fim de determinar a __Orientation__ que será necessária na Play 

-}
goToFood :: Player -> [Corridor] -> Orientation 
goToFood (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) (x:xs) = whichOrientation (a,b) best
  where  lc       = coordsFoodBig (x:xs) 
         coordBig = bestCoord lc (a,b)
         poss     = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
         lp       = notWall poss (x:xs)
         best     = bestCoord lp coordBig
          

{- | Função que dada uma lista de jogadores, devolve a lista com as coordenadas de cada jogador

*Esta função torna-se necessária para que possamos selecionar as coordenadas de todos os fantasmas presentes no labirinto

-}
getCoordsL :: [Player] -> [Coords]
getCoordsL []     = []
getCoordsL (p:ps) = getPlayerCoords p : getCoordsL ps 


{- | Função que dado o Pacman e o estado do jogo, procura qual fantasma está mais próximo e determina qual deve ser a orientação utilizada na próxima jogada para que o pacman vá até este fantasma

@
coordsGhosts = getCoordsL $ getGhosts (p:ps)
@
*Nesta etapa, selecionamos os fantasmas da lista de jogadores e, a seguir, selecionamos suas coordenadas no labirinto

@
coordToGo    = bestCoord (coordsGhosts) (a,b)
@
*Precisamos saber qual o fantasma que está mais próximo do Pacman, para que o Pacman ao estar Mega possa perseguir o fantasma
*Com isto, aplicamos a função x da Tarefa 5 para selecionar a coordenada na qual o Pacman precisa de menos jogadas para alcançar

@
lc           = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
@
*Estas são as jogadas existentes para o Pacman: D, U, R, L respetivamente. 

@
poss         = notWall lc (x:xs)
@
*Nesta etapa, são selecionadas apenas as coordenadas que não contém __Wall__ das coordenadas relativas às jogadas existentes

@
best         = bestCoord poss coordToGo 
@
*Aqui, utiliza-se a função da Tarefa 5 para comparar as coordenadas possíveis que o Pacman pode alcançar, ou seja, as que não possuem __Wal__ com a coordenada do fantasma mais próximo e selecionar qual destas coordenadas requer o menor número de jogadas para o Pacman 

@
whichOrientation (a,b) best
@
*E, com isto, ao comparar a coordenada atual do Pacman com a que ele quer ir na pŕoxima jogada, seleciona-se a __Orientation__ necessária para Play 
-}
chaseGhosts :: Player -> State -> Orientation
chaseGhosts (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) (State (x:xs) (p:ps) l) = whichOrientation (a,b) best 
    where coordsGhosts = getCoordsL $ getGhosts (p:ps)
          coordToGo    = bestCoord (coordsGhosts) (a,b)
          lc           = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
          poss         = notWall lc (x:xs)
          best         = bestCoord poss coordToGo 


{- | Função que dado o Pacman e o estado do jogo, procura qual fantasma está mais próximo e determina qual deve ser a orientação utilizada na próxima jogada para que o pacman se distancie deste fantasma

@
coordsGhosts = getCoordsL $ getGhosts (p:ps)
@
*Seleciona as coordernadas dos fantasmas presentes no labirinto
@
closest      = bestCoord (coordsGhosts) (a,b)
@
*Utiliza-se a função da Tarefa 5 para selecionar a coordenada do fantasma que está mais próximo do Pacman 

@
lc           = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
@
*Estas são as jogadas existentes para o Pacman: D, U, R, L respetivamente. 

@
poss         = notWall lc (x:xs)
@
*Nesta etapa, são selecionadas apenas as coordenadas que não contém __Wall__ das coordenadas relativas às jogadas existentes

@
best         = betterRun poss closest
@
*Como o Pacman precisa ir para uma coordenada mais longe que a coordenada do fantasma mais próximo, utiliza-se a função betterRun presente na Tarefa 5 para determinar qual a próxima coordenada do Pacman

@
whichOrientation (a,b) best
@
*E, com isto, ao comparar a coordenada atual do Pacman com a que ele quer ir na pŕoxima jogada, seleciona-se a __Orientation__ necessária para Play 
-}
getOut :: Player -> State -> Orientation 
getOut (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) (State (x:xs) (p:ps) l) = whichOrientation (a,b) best 
   where  coordsGhosts = getCoordsL $ getGhosts (p:ps)
          closest      = bestCoord (coordsGhosts) (a,b)
          lc           = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
          poss         = notWall lc (x:xs)
          best         = betterRun poss closest