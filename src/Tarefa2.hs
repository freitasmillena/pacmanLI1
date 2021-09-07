{-|
Module: Tarefa2
Description: Módulo Haskell que determina o efeito de uma jogada 
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>
Um módulo Haskell que determina o efeito de uma jogada no estado do jogo dada a descrição do estado do jogo e a jogada de um dos jogadores 

= Introdução
A tarefa 2 é a tarefa onde todas as jogadas são aplicadas a um estado de jogo. É nesta tarefa que se possibilita que os fantasmas e o pacman consigam realizar as jogadas. Quando dado um estado de jogo e uma jogada de um dos jogadores, o resultado será um novo estado de jogo que irá incluir a atualização do labirinto consuante a jogada realizada, e a atualização de todos os estados dos jogadores como consequência da jogada realizada.

=Objetivo 

==/Fase 1/
O nosso objetivo final desta tarefa era conseguir com que fosse possível realizar jogadas com o pacman, com o menor número de bugs possível e seguindo as regras contidas no enunciado. 

Em relação ao processamento da jogada, esta deve ser efetuada considerando que:

*Se a __jogada tiver uma orientação diferente__ da orientação do jogador, este último apenas __altera a sua orientação__ para a desejada e permanece nas mesmas coordenadas. 
*Caso contrário, se possível, ou seja, caso __não exista uma parede (Wall)__, o jogador deve movimentar-se e __alterar as suas coordenadas__ consuante a jogada desejada. 

As ações induzidas pelas ações realizadas depende do tipo de peça existente nas coordenadas para qual a jogada leva o jogador:

*Se não existe __nenhuma peça__, __nada acontece__
*Se existir uma __comida pequena__, a pontuação do jogador deverá ser atualizada com __+1 ponto__;
*Se existir uma __comida grande__, a pontuação do jogador deverá ser atualizada com __+5 pontos__ e o estado do jogador também deverá ser alterado para __Mega__. Considerando que o estado do pacman se altera para o Mega, todos os __fantasmas__ existintes no labirinto alteram o seu estado para __Dead__ e diminuem a sua __velocidade para metade__.
*Se existir um fantasma vivo __(Alive)__,

                          1.Caso o jogador ainda tenha vidas, deve perder uma vida.
                          2.Caso contrário, o jogador deve atualizar o seu estado para Dying.

*Se existir um fantasma morto __(Dead)__, o jogador deve “comer” o fantasma e a pontuação do mesmo deverá ser atualizada com __+10 pontos__. Quando os __fantasmas__ são "comidos" devem __voltar à casa dos fantasmas__ e voltar a ficarem vivos, ou seja, o seu estado tem que ser alterado para __Alive__.
*Se existir um espaço vazio junto ao limite do labirinto, o que vai corresponder a um __túnel__, pelo que, o __jogador deve ser transportado para o lado oposto do labirinto__.

==/Fase 2/
Consuante o enunciado da segunda fase tivemos que acrescentar algumas condições a esta tarefa. As modificações feitas nesta fase têm o objetivo de fazer com que os fantasmas também possam efetuar jogadas e acrescentar a passagem do tempo às jogadas realizadas para a tarefa 4, e foram feitas seguindo as regras contidas no enunciado:

*A __boca do Pacman deve abrir e fechar__ alternadamente entre cada jogada;
*Se o __Pacman__ estiver em estado o __Mega__, o tempo mega __(timeMega) deve sofrer aletrações__ em cada jogada;
*Se o __Pacman__ estiver em estado o __Mega__ e se o tempo mega __(timeMega) for <=0__, o mesmo deve voltar ao estado __Alive__;
*Se não existir um Pacman em modo __Mega__, os fantasmas devem voltar ao estado __Alive__; 

=Discussão e conclusão

Esta tarefa foi realizada seguindo as indicações indicadas no enunciado. Juntamos o necessário para conseguir realizar cada jogada com sucesso, tentado cumprir as condições definidas no máximo de cenários possível. Isto é, tentamos não limitar o funcionamento do nosso jogo a mapas ou situações específicas mas sim a todo o tipo de situações com que nos deparamos. Também foi uma ótima tarefa para perceber melhor sobre o jogo e sobre os tipos que nos forneceram.

Em conclusão, ficamos bastante felizes com o resultado final desta tarefa. Após a segunda fase do trabalho, quando tivemos que tornar esta tarefa mais rica, tivemos a oportunidade de corrigir erros que faziam com que nem todas as possibilidades fossem levadas em consideração ou que não cumpriam o que era suposto. Portanto, foi um grande desafio levando em conta a quantidade de possibilidades. Tivemos que, além de estudar o jogo, utilizar nossa imaginação.

-}

module Tarefa2 where 

import Types
import Tarefa1
import FileUtils



{- | Função que dada uma jogada, um labirinto e uma lista de jogadores, devolve o jogador de ID j atualizado

@
if o == getPlayerOrientation (findPlayer j (p:ps)) then  case o of 
                                     R -> moveAfterGhostSide (Move j o) (x:xs) (p:ps) 1 R
                                     L -> moveAfterGhostSide (Move j o) (x:xs) (p:ps) (-1) L
                                     U -> moveAfterGhostVertical (Move j o) (x:xs) (p:ps) (-1) U
                                     D -> moveAfterGhostVertical (Move j o) (x:xs) (p:ps) 1 D
else changeOrientation (Move j o) (x:xs) (findPlayer j (p:ps)) o
@
*A primeira condição testa se a orientação recebida na jogada é a mesma que o jogador possui
*Se for aplica a função de acordo com a orientação horizontal ou vertical
*Caso contrário muda apenas a orientação do jogador para a orientação desejada da jogada
-}
movePlay :: Play -> Maze -> [Player] -> Player
movePlay (Move j o) (x:xs) (p:ps) = if o == getPlayerOrientation (findPlayer j (p:ps)) then  case o of 
                                                                                               R -> moveAfterGhostSide (Move j o) (x:xs) (p:ps) 1 R
                                                                                               L -> moveAfterGhostSide (Move j o) (x:xs) (p:ps) (-1) L
                                                                                               U -> moveAfterGhostVertical (Move j o) (x:xs) (p:ps) (-1) U
                                                                                               D -> moveAfterGhostVertical (Move j o) (x:xs) (p:ps) 1 D
                                                                                               
                                    else changeOrientation (Move j o) (x:xs) (findPlayer j (p:ps)) o

{- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação às orientações para __esquerda__ e __direita__ 
 
@
if isthereGhost' (p:ps) (a,b) == True
@
*A função analisa se, nas coordenadas para onde a jogada pretende mover o pacman existe um fantasma. 

@
if idpieceCoord (x:xs) (a,b) == Food Big then collideGhostSide mega i ori
else collideGhostSide (findPlayer j (p:ps)) i ori
@
*Se existir um fantasma, entra outra condição para saber se existe __Food Big__ nas coordenadas que o jogador pretende ir, caso exista é invocada a função que movimenta o pacman consuante a colisão com o mesmo, sendo que se utiliza a função stateMega para alterar o estado do pacman para Mega . 
*Caso não exista __Food Big__, é invocada a função que movimenta o pacman consuante a colisão com um fantasma

@
else movePlayerSide (Move j o) (x:xs) (findPlayer j (p:ps)) i ori 
@
*Se não existir, é invocada a função que movimenta o pacman consuante a peça que existe nessas coordenadas

@
where (a,b) = coordB (getCoords (findPlayer j (p:ps))) i
@
*Coordenada que o jogador pretende ir
-}

moveAfterGhostSide:: Play -> Maze ->[Player] -> Int -> Orientation -> Player
moveAfterGhostSide (Move j o) (x:xs) (p:ps) i ori = if isthereGhost' (p:ps) (a,b) == True then 
                                                               if idpieceCoord (x:xs) (a,b) == Food Big then collideGhostSide mega (p:ps) i ori
                                                               else collideGhostSide (findPlayer j (p:ps)) (p:ps) i ori
                                                    else movePlayerSide (Move j o) (x:xs) (findPlayer j (p:ps)) i ori 
                                                        where (a,b) = coordB (getCoords (findPlayer j (p:ps))) i
                                                              mega = stateMega (findPlayer j (p:ps))


{- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação às orientações para __cima__ e __baixo__ 

*A função tem o mesmo objetivo da função __moveAfterGhostSide__ porém para jogadas na vertical 
-}
moveAfterGhostVertical:: Play -> Maze -> [Player] -> Int -> Orientation -> Player
moveAfterGhostVertical (Move j o) (x:xs) (p:ps) i ori = if isthereGhost' (p:ps) (a,b) == True then 
                                                          if idpieceCoord (x:xs) (a,b) == Food Big then collideGhostVertical mega (p:ps) i ori
                                                               else collideGhostVertical (findPlayer j (p:ps)) (p:ps) i ori
                                                        else movePlayerVertical (Move j o) (x:xs) (findPlayer j (p:ps)) i ori
                                                   where (a,b) = coordA (getCoords (findPlayer j (p:ps))) i
                                                         mega = stateMega (findPlayer j (p:ps))

-- | Função que dado um player pacman altera o estado do pacman para Mega e altera o timeMega para 10 sem alterar as suas coordenadas 
--
stateMega :: Player -> Player
stateMega (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) = (Pacman (PacState (id, (a,b), v, or, points + 5, lifes) 10 openClosed Mega))

-- | Função que verifica se ẽxiste um player com esta coodernada e, se houver, verifica se é fantasma
--
isthereGhost' :: [Player] -> Coords -> Bool
isthereGhost' [] _ = False
isthereGhost' (p:ps) (a,b)
  |istherePlayer (p:ps) (a,b) == True = isitGhost (findPlayerCoord (p:ps) (a,b))
  |otherwise = False

{- | Função quando o pacman colide com um fantasma. Esta função é em relação às orientações __esquerda__ e __direita__

@
if pacmanMode /= Mega
@
*A função analisa se o pacman está em estado Mega

@
if lifes == 0 then (Pacman (PacState (id, (a,b+i), v, ori, points, lifes) timeMega openClosed Dying))
else (Pacman (PacState (id, (a,b+i), v, ori, points, lifes -1) timeMega openClosed pacmanMode))
@
*Em caso negativo, o pacman perde 1 vida (no caso de ter 0 vidas o pacman morre)

@
else (Pacman (PacState (id, (a,b+i), v, ori, points+10, lifes) timeMega openClosed pacmanMode))  
@
*Caso contrário, o pacman come o fantasma e ganha 10 pontos. 
-}                                                     
collideGhostSide :: Player -> [Player] -> Int -> Orientation -> Player 
collideGhostSide (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) (p:ps) i ori = if pacmanMode /= Mega then
                                                                                                                 if lifes == 0 then (Pacman (PacState (id, (a,b+i), v, ori, points, lifes) timeMega openClosed Dying))
                                                                                                                 else (Pacman (PacState (id, (a,b+i), v, ori, points, lifes -1) timeMega openClosed pacmanMode))
                                                                                                             else (Pacman (PacState (id, (a,b+i), v, ori, (points+(10*qtGhosts)), lifes) timeMega openClosed pacmanMode))  
     where qtGhosts = length (getGhosts (findPlayerscoord (p:ps) coords))
           coords = coordB (a,b) i

{- | Função quando o pacman colide com um fantasma. Esta função é em relação às orientações __cima__ e __baixo__

*A função tem o mesmo objetivo da função __collideGhostSide__ porém para jogadas na vertical 
-}   
collideGhostVertical :: Player -> [Player] -> Int -> Orientation -> Player 
collideGhostVertical (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) (p:ps) i ori = if pacmanMode /= Mega then
                                                                                                              if lifes == 0 then (Pacman (PacState (id, (a+i,b), v, ori, points, lifes) timeMega openClosed Dying))
                                                                                                              else (Pacman (PacState (id, (a+i,b), v, ori, points, lifes -1) timeMega openClosed pacmanMode))
                                                                                                          else (Pacman (PacState (id, (a+i,b), v, ori, (points+(10*qtGhosts)), lifes) timeMega openClosed pacmanMode)) 
        where qtGhosts = length (getGhosts (findPlayerscoord (p:ps) coords))
              coords = coordA (a,b) i

{- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação às orientações para __esquerda__ e __direita__

@
case idpieceCoord (x:xs) (a,b+i) of
@
*A função analisa qual a peça que se encontra nas coordenadas para onde a jogada pretende mover o pacman

@
Wall -> (Pacman (PacState (j, (a,b), v, ori, points, lifes) timeMega openClosed pacmanMode))
@
*Se existir uma parede, o pacman fica igual

@
Food Big -> (Pacman (PacState (j, (a,b+i), v, ori, points + 5, lifes) 10 openClosed Mega))
@
*Se existir uma Food Big, o pacman ganha 5 pontos, o timeMega altera para 10, e o seu estado altera para Mega. Aĺém disto, as suas coordenadas alteram consuante a orientação que este se move.

@
Food Little -> (Pacman (PacState (j, (a,b+i), v, ori, points + 1, lifes) timeMega openClosed pacmanMode))
@
*Se existir uma Food Little, o pacman ganha 1 ponto, e as coordenadas do mesmo alterma consoante a orientação que este se move

@
Empty -> if o == R then 
      if (a,b) == (a,(length x)-1)
      then (Pacman (PacState (j, (a,0), v, ori, points, lifes) timeMega openClosed pacmanMode))
      else (Pacman (PacState (j, (a,b+i), v, ori, points, lifes) timeMega openClosed pacmanMode))
else 
      if (a,b) == (a,0) 
      then (Pacman (PacState (j, (a,(length x)-1), v, ori, points, lifes) timeMega openClosed pacmanMode))
      else (Pacman (PacState (j, (a,b+i), v, ori, points, lifes) timeMega openClosed pacmanMode))
@
*Se existir Empty, verifica se é apenas um espaço vazio ou se é um túnel
*Caso seja um túnel, o jogador é transportado para o outro lado do labirinto por este túnel
*Caso contrário, apenas se alteram as suas coordenadas consuante a jogada
-}
movePlayerSide :: Play -> Maze -> Player -> Int -> Orientation -> Player  
movePlayerSide (Move j o) (x:xs) (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) i ori = case idpieceCoord (x:xs) (a,b+i) of
                                                                                                                              Wall -> (Pacman (PacState (j, (a,b), v, ori, points, lifes) timeMega openClosed pacmanMode))
                                                                                                                              Food Big -> (Pacman (PacState (j, (a,b+i), v, ori, points + 5, lifes) 10 openClosed Mega))
                                                                                                                              Food Little -> (Pacman (PacState (j, (a,b+i), v, ori, points + 1, lifes) timeMega openClosed pacmanMode))
                                                                                                                              Empty -> if o == R then 
                                                                                                                                              if (a,b) == (a,(length x)-1)
                                                                                                                                              then (Pacman (PacState (j, (a,0), v, ori, points, lifes) timeMega openClosed pacmanMode))
                                                                                                                                              else (Pacman (PacState (j, (a,b+i), v, ori, points, lifes) timeMega openClosed pacmanMode))
                                                                                                                                        else 
                                                                                                                                            if (a,b) == (a,0) 
                                                                                                                                            then (Pacman (PacState (j, (a,(length x)-1), v, ori, points, lifes) timeMega openClosed pacmanMode))
                                                                                                                                            else (Pacman (PacState (j, (a,b+i), v, ori, points, lifes) timeMega openClosed pacmanMode))
movePlayerSide (Move j o) (x:xs) (Ghost (GhoState (id, (a,b), v, or, points, lifes) ghostMode)) i ori = case idpieceCoord (x:xs) (a,b+i) of
                                                                                                          Wall -> (Ghost (GhoState (id, (a,b), v, ori, points, lifes) ghostMode))
                                                                                                          Empty -> if o == R then 
                                                                                                                          if (a,b) == (a,(length x)-1)
                                                                                                                          then (Ghost (GhoState (id, (a,0), v, ori, points, lifes) ghostMode))
                                                                                                                          else (Ghost (GhoState (id, (a,b+i), v, ori, points, lifes) ghostMode))
                                                                                                                    else 
                                                                                                                        if (a,b) == (a,0) 
                                                                                                                        then (Ghost (GhoState (id, (a,(length x)-1), v, ori, points, lifes) ghostMode))
                                                                                                                        else (Ghost (GhoState (id, (a,b+i), v, ori, points, lifes) ghostMode))
                                                                                                          _-> (Ghost (GhoState (id, (a,b+i), v, ori, points, lifes) ghostMode))

{- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação às orientações para __cima__ e __baixo__

*A função tem o mesmo objetivo da função __movePlayerSide__ porém para jogadas na vertical, com a exceção de quando a peça é __Empty__, pois neste tipo de jogadas o jogador não consegue aceder aos túneis, devido ao labirinto ser limitado lateralmente por paredes.
-}

movePlayerVertical :: Play -> Maze -> Player -> Int -> Orientation -> Player  
movePlayerVertical (Move j o) (x:xs) (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) i ori = case idpieceCoord (x:xs) (a+i,b) of 
                                                                                                                                  Wall -> (Pacman (PacState (j, (a,b), v, ori, points, lifes) timeMega openClosed pacmanMode))
                                                                                                                                  Food Big -> (Pacman (PacState (j, (a+i,b), v, ori, points + 5, lifes) 10 openClosed Mega))
                                                                                                                                  Food Little -> (Pacman (PacState (j, (a+i,b), v, ori, points + 1, lifes) timeMega openClosed pacmanMode))
                                                                                                                                  Empty -> (Pacman (PacState (j, (a+i,b), v, ori, points, lifes) timeMega openClosed pacmanMode))
movePlayerVertical (Move j o) (x:xs) (Ghost (GhoState (id, (a,b), v, or, points, lifes) ghostMode)) i ori = case idpieceCoord (x:xs) (a+i,b) of
                                                                                                               Wall -> (Ghost (GhoState (id, (a,b), v, ori, points, lifes) ghostMode))
                                                                                                               _-> (Ghost (GhoState (id, (a+i,b), v, ori, points, lifes) ghostMode))

-- | Função que dado uma jogada, um labirinto, um jogador e uma orientação, devolve o jogador após a jogada. Neste caso o jogador apenas muda a orientação e __não__ avança no labirinto, pelo que apenas muda a __orientação__ para a desejada.
--
changeOrientation :: Play -> Maze -> Player -> Orientation -> Player 
changeOrientation (Move j o) (x:xs) (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) ori =  (Pacman (PacState (id, (a,b), v, ori, points, lifes) timeMega openClosed pacmanMode))
changeOrientation (Move j o) (x:xs) (Ghost (GhoState (id, (a,b), v, or, points, lifes) ghostMode)) ori = (Ghost (GhoState (id, (a,b), v, ori, points, lifes) ghostMode))
                                                                                                              
-- | Função que dado um corredor e uma coordenada, devolve a peça equivalente 
--
idpieceCorridor :: Corridor -> Coords -> Piece 
idpieceCorridor [] _ = Empty
idpieceCorridor (x:xs) (a,b) 
    |b == 0 = x
    |otherwise = idpieceCorridor xs (a,b-1)

-- | Função que dado um labirinto e uma coordenada, devolve a peça correspondente 
--
idpieceCoord :: Maze -> Coords -> Piece
idpieceCoord (x:xs) (a,b) = idpieceCorridor (idPiecePosition (x:xs) a) (a,b)

-- | Função que seleciona o corredor de um labirinto equivalente às coordenadas dadas 
--
corridorChangePiece :: Maze -> Coords -> [Piece] 
corridorChangePiece (x:xs) (a,b) = idPiecePosition (x:xs) a

{- | Função que dado um labirinto, um corredor e uma coordenada troca a peça equivalente por __Empty__

@
superiormaze = fst (splitAt a (x:xs))
@
*Divide o labirinto verticalmente quando encontra o corredor relativo à posição __a__ e seleciona o primeiro corredor desta lista 

@
inferiormaze = tail (snd (splitAt a (x:xs)))
@
*Divide o labirinto verticalmente quando encontra o corredor relativo à posição __a__, seleciona a segunda lista de listas e desta lista retorna apenas a tail

@
corridorChange = [fst (splitAt b (head (snd (splitAt a (x:xs))))) ++ [Empty] ++ tail (snd (splitAt b (head (snd (splitAt a (x:xs))))))]
@
*A primeira parte: Divide o labirinto verticalmente quando encontra o corredor relativo à posição __a__, seleciona a segunda lista de listas e desta lista retorna apenas a head. Com isto divide horizontalmente esta lista quando atinge a posição relativa a __b__ e por fim retorna a primeira lista de listas.
*Adiciona a peça __Empty__ 
*A ultima parte: Divide o labirinto verticalmente quando encontra o corredor relativo à posição __a__, seleciona a segunda lista de listas e desta lista retorna apenas a head. Com isto divide horizontalmente esta lista quando atinge a posição relativa a __b__ e por fim retorna a segunda lista de listas.
-}
changePieces :: Maze -> [Piece] -> Coords -> Maze 
changePieces (x:xs) y (a,b) = superiormaze ++ corridorChange ++ inferiormaze
                           where superiormaze = fst (splitAt a (x:xs))
                                 inferiormaze = tail (snd (splitAt a (x:xs)))
                                 corridorChange = [fst (splitAt b (head (snd (splitAt a (x:xs))))) ++ [Empty] ++ tail (snd (splitAt b (head (snd (splitAt a (x:xs))))))]

-- | Função que altera o labirinto caso a posição após a jogada tenha comida pequena ou comida grande
--
updateMaze :: Maze -> Player -> Maze 
updateMaze [] _ = []
updateMaze (x:xs) (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) 
   |idpieceCoord (x:xs) (a,b) == Food Big  = changePieces (x:xs) corridorPiece (a,b)
   |idpieceCoord (x:xs) (a,b) == Food Little  = changePieces (x:xs) corridorPiece (a,b)
   |otherwise = (x:xs)
      where corridorPiece = corridorChangePiece (x:xs) (a,b)

-- | Função que dado um par de coordenadas, devolve um par de coordenadas, alterando-o consuante a orientação da jogada. Esta função é em relação às orientações para __esquerda__ e __direita__
--
coordA :: Coords -> Int -> Coords
coordA (a,b) i = (a+i,b)

-- | Função que dado um par de coordenadas, devolve um par de coordenadas, alternando-o consuante a orientação da jogada. Esta função é em relação às orientações para __cima__ e __baixo__
--
coordB :: Coords -> Int -> Coords
coordB (a,b) i = (a,b+i)


{- | Função que move os fantasmas comidos de volta para a casa dos fantasmas e voltam a estar vivos

@
if even (length (x:xs))
@
*Foi necessário separar condições para quando a altura é par ou ímpar, pois o local onde os fantasmas se encontram na casa de fantasmas muda

@
(middleMaze (x:xs)+n,middleWidth (x:xs))
@
*Caso seja altura par, o n = 0 e caso seja altura ímpar o n=1
-}                     
ghostHome :: Maze -> [Player] -> [Player]
ghostHome _ [] = []
ghostHome (x:xs) ((Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)):gs) = if even (length (x:xs)) then (Ghost (GhoState (id, (middleMaze (x:xs),middleWidth (x:xs)), v , o, points, lifes) Alive)) : ghostHome (x:xs) gs else (Ghost (GhoState (id, ((middleMaze(x:xs) +1),middleWidth (x:xs)), v, o, points, lifes) Alive)) : ghostHome (x:xs) gs 
                                                                                                                                                                  
 
-- | Função que dado o ID identifica o estado do jogador desejado para realizar a jogada
--
findPlayer :: Int -> [Player] -> Player 
findPlayer j [] = error "Player não encontrado"
findPlayer j (p:ps) = if j == getPlayerID p then p else findPlayer j ps 

{- | Função que atualiza o estado ao substituir o estado do jogador após sua jogada

@
if id == getPlayerID p then (State (x:xs) (player:ps) l)
else (State (x:xs) (p : playersatt) l) 
@

@
playersatt = (getPlayers (updateState (State (x:xs) ps l) player))
@

*Se o id do jogador for igual ao id do primeiro jogador da lista, então troca-se o estado do primeiro jogador da lista pelo mais atualizado
*Caso contrário, repete-se o primeiro jogador da lista e procura o jogador no resto da lista
*A mesma coisa é feita para os fantasmas
-}
updateState :: State -> Player -> State 
updateState (State (x:xs) (p:ps) l) player@(Pacman (PacState (id, _, _, _, _, _) timeMega openClosed pacmanMode)) = if id == getPlayerID p then (State (x:xs) (player:ps) l)
                                                                                                                    else (State (x:xs) (p : playersatt) l) 
                                                                                                                     where playersatt = (getPlayers (updateState (State (x:xs) ps l) player))
updateState (State (x:xs) (p:ps) l) ghost@(Ghost (GhoState (id, _, _ ,_, _, _) ghostMode)) = if id == getPlayerID p then (State (x:xs) (ghost:ps) l)
                                                                                             else (State (x:xs) (p : ghostsatt) l) 
                                                                                                                     where ghostsatt = (getPlayers (updateState (State (x:xs) ps l) ghost))

-- | Função que dado um State, devolve a lista de jogadores
--
getPlayers :: State -> [Player]
getPlayers (State (x:xs) [] l) = []
getPlayers (State (x:xs) (p:ps) l) = (p:ps)   

-- | Função que extrai as coordenadas de um jogador
--
getCoords :: Player -> Coords
getCoords (Pacman (PacState (_, (a,b), _, _, _, _) timeMega openClosed pacmanMode)) = (a,b)
getCoords (Ghost (GhoState (_, (a,b), _ ,_, _, _) ghostMode)) = (a,b)

-- | Função que retorna o jogador com a coordenada pedida
--
findPlayerCoord :: [Player] -> Coords -> Player
findPlayerCoord (p:ps) (a,b) = if (a,b) == getCoords p then p else findPlayerCoord ps (a,b) 

-- | Função que retorna lista de jogadores com a coordenada pedida
--
findPlayerscoord :: [Player] -> Coords -> [Player]
findPlayerscoord [] _ = []
findPlayerscoord (p:ps) (a,b) = if (a,b) == getCoords p then p:findPlayerscoord ps (a,b) else findPlayerscoord ps (a,b)

-- | Função que retorna o pacmode 
--
pacMode :: Player -> PacMode 
pacMode (Pacman (PacState (_, (a,b), _, _, _, _) timeMega openClosed pacmanMode)) = pacmanMode 

-- | Função que confirma se um player é ou não um fantasma
--
isitGhost :: Player -> Bool 
isitGhost (Pacman (PacState (_, _, _, _, _, _) _ _ _)) = False
isitGhost (Ghost (GhoState (_, _, _ ,_, _, _) _)) = True 

{- | Função que dado uma lista de jogada, um labirinto e uma lista de jogadores confirma se há ou não jogador com esta coordenada na lista. Caso exista, confirma-se se o jogador é ou não um fantasma

@
isitGhost (findPlayerCoord (p:ps) coord)
@
*Caso haja jogador com as coordenadas após a jogada, confirma se é um fantasma

@
coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))
@
*Seleciona as coordenadas do jogador após a jogada que ele pretende efetuar
-}
isthereGhost :: Play -> Maze -> [Player] -> Bool
isthereGhost (Move j o) (x:xs) (p:ps) = case istherePlayer (p:ps) coord of 
                                           True -> isitGhost (findPlayerCoord (p:ps) coord)
                                           False -> False
  where coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))

-- | Função que dado uma lista de jogada, um labirinto e uma lista de jogadores confirma se há ou não jogador com esta coordenada na lista. Caso exista, confirma-se se o jogador é ou não um pacman
--
istherePac :: Play -> Maze -> [Player] -> Bool
istherePac (Move j o) (x:xs) (p:ps) = case istherePlayer (p:ps) coord of 
                                           True -> if isitGhost (findPlayerCoord (p:ps) coord) == False then True else False 
                                           False -> False
  where coord = nextCoord (Move j o) (findPlayer j (p:ps))

nextCoord :: Play -> Player -> Coords
nextCoord (Move j o) (Ghost (GhoState (id, (a,b), v, ori, points, lifes) ghostMode)) = if o == ori then case o of 
                                                                                                          R -> (a,b+1)
                                                                                                          L -> (a,b-1)
                                                                                                          U -> (a-1,b)
                                                                                                          D -> (a+1,b)
                                                                                       else (a,b)

-- | Função que dado uma lista de jogadores e uma coordenada, confirma se há ou não jogador com esta coordenada na lista
--
istherePlayer :: [Player] -> Coords -> Bool
istherePlayer [] _ = False
istherePlayer (p:ps) (a,b) 
  |getCoords p == (a,b) = True
  |otherwise = istherePlayer ps (a,b)

{- | Função que dado um labirinto e dois jogadores, um pacman e um ghost, atualiza o estado do ghost consuante o estado do pacman

@
if pacmanMode == Mega then ghostHome (x:xs) ghost
else ghost 
@
*Se o pacman estiver em estado Mega, aplica a função ghostHome ao fantasma, para que volte para a casa de fantasmas e vivo
*Caso contrário, o ghost permanece inalterado

-}

stateGhost :: Maze -> Player -> [Player] -> [Player]
stateGhost _ _ [] = []
stateGhost (x:xs) (Pacman (PacState (_, _, _, _, _, _) timeMega openClosed pacmanMode)) ghost@((Ghost (GhoState (_, _, _ ,_, _, _) ghostMode)):gs) = if pacmanMode == Mega then ghostHome (x:xs) ghost
                                                                                                                                                     else ghost 
                                                                                                                                                
-- | Função que recebe uma lista só com fantasmas e muda os ghostModes para Dead e suas velocidades reduzidas para a metade
--
ghostDead :: [Player] -> [Player] 
ghostDead [] = []
ghostDead ((Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)):gs) = case o of 
                                                                                 R -> (Ghost (GhoState (id, (a,b), v/2, L, points, lifes) Dead)) : ghostDead gs 
                                                                                 L -> (Ghost (GhoState (id, (a,b), v/2, R, points, lifes) Dead)) : ghostDead gs 
                                                                                 U -> (Ghost (GhoState (id, (a,b), v/2, D, points, lifes) Dead)) : ghostDead gs 
                                                                                 D -> (Ghost (GhoState (id, (a,b), v/2, U, points, lifes) Dead)) : ghostDead gs 
                                                                                 Null -> (Ghost (GhoState (id, (a,b), v/2, Null, points, lifes) Dead)) : ghostDead gs
-- | Função que recebe uma lista só com fantasmas e muda os ghostModes para Alive e suas velocidades para o normal
--
ghostAlive :: [Player] -> [Player] 
ghostAlive [] = []
ghostAlive ((Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)):gs) = (Ghost (GhoState (id, (a,b), v, o, points, lifes) Alive)) : ghostAlive gs 


-- | Função que dado uma lista de players, devolve os que são fantasmas
--
getGhosts :: [Player] -> [Player]
getGhosts [] = []
getGhosts (p:ps) 
  |isitGhost p == True = p : getGhosts ps 
  |otherwise = getGhosts ps 


{- | Função que recebe um estado, uma lista de fantasmas e devolve o estado com os fantasmas trocados na lista

@
if getPlayerID g == getPlayerID p then (State (x:xs) (g:ghostsatt) l)
else (State (x:xs) (p:ghostsatt') l)
@
*Caso o primeiro jogador seja um fantasma, seu estado de jogador é trocado para o que o fantasma fica quando o pacman torna-se Mega e procura-se pelos outros fantasmas no resto da lista
*Caso contrário, repete-se o primeiro jogador e procura-se no resto da lista dos jogadores
-}
updatePlayersGhosts :: State -> [Player] -> State 
updatePlayersGhosts (State [] (p:ps) l) _ = (State [] (p:ps) l)
updatePlayersGhosts (State (x:xs) [] l) _ = (State (x:xs) [] l)
updatePlayersGhosts (State (x:xs) (p:ps) l) [] = (State (x:xs) (p:ps) l)
updatePlayersGhosts (State (x:xs) (p:ps) l) (g:gs) = if getPlayerID g == getPlayerID p then (State (x:xs) (g:ghostsatt) l)
                                                     else (State (x:xs) (p:ghostsatt') l)
  where ghostsatt = getPlayers (updatePlayersGhosts (State (x:xs) ps l) gs)
        ghostsatt' = getPlayers (updatePlayersGhosts (State (x:xs) ps l) (g:gs))

{- | Função que dado um estado e um jogador, devolve um novo estado dependendo do modo que este jogador se encontra

@
if pacmanMode == Mega then updatePlayersGhosts newStatePlayer lghosts
else newStatePlayer
@
*Se o pacman estiver em estado Mega, o jogador tem seu estado trocado pelo após a jogada e os ghosts passarão a estar Dead e com velocidade reduzida para a metade
*Caso contrário, apenas o estado do jogador é trocado pelo após a jogada

@
newStatePlayer = updateState (State (x:xs) (p:ps) l) player
@
*Troca o estado do jogador pelo estado novo após a jogada
            
@
lghosts = ghostDead (getGhosts (p:ps))
@
*Seleciona os fantasmas da lista de jogadores e muda seus estados para Dead e reduz suas velocidades para a metade 
-}
newState :: State -> Player -> State 
newState (State (x:xs) (p:ps) l) player@(Pacman (PacState (_, (a,b), _, _, _, _) _ _ pacmanMode)) = case isitFood (a,b) (x:xs) of
                                                                                                         True -> updatePlayersGhosts newStatePlayer lghosts
                                                                                                         False -> if pacmanMode == Mega then newStatePlayer 
                                                                                                                  else newStatePlayer'
      where newStatePlayer = updateState (State maze (p:ps) l) player                                           
            lghosts = ghostDead (getGhosts (p:ps))
            lghosts' = ghostAlive (getGhosts (p:ps))
            newStatePlayer' = updatePlayersGhosts newStatePlayer lghosts'
            maze = updateMaze (x:xs) player
            

isitFood :: Coords -> Maze -> Bool
isitFood _ [] = False
isitFood (a,b) (x:xs) = idpieceCoord (x:xs) (a,b) == Food Big 


{- | Função play para jogadas com o pacman


@
case coordsFoodLittle (x:xs) of
@
*Caso a lista das coordenadas das comidas do labirinto seja vazia, significa que já não há mais comidas e, portanto, o jogador venceu. Isto retorna um error "You win!";
*Caso contrário, a jogada é contabilizada.

@
if isthereGhost (Move j o) (x:xs) (p:ps) == True then newStateGhost
else newState (State maze (p:ps) l) player
@
*Caso exista fantasma na posição que o pacman deseja ir, então devolve o estado atualizado com os estados dos fantasmas que foram comidos alterados, caso tenha acontecido, com o estado do pacman alterado e com o labirinto alterado caso tenha comido alguma comida
*Caso contrário, devolve o estado atualizado com os estados dos fantasmas mudados caso o pacman tenha se tornado Mega, com o estado do pacman atualizado e com o labirinto atualizado caso tenha comido alguma comida

@
player = movePlay (Move j o) (x:xs) (p:ps)
@
*Estado do jogador após efetuar a jogada

@
maze = updateMaze (x:xs) player 
@
*Labirinto atualizado após a jogada 

@
coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))
@
*Seleciona as coordenadas do jogador após a jogada

@
ghostNew = stateGhost (x:xs) (findPlayer j (p:ps)) (findPlayerscoord (p:ps) coord) 
@
*Procura se há fantasmas com a coordenada que o jogador quer ir, procura o pacman na lista de jogadores e atualiza o estado do(s) fantasma(s) consuante o estado do pacman

@
newStateGhost = updatePlayersGhosts (newState (State maze (p:ps) l) player) ghostNew
@
*Recebe o novo estado do jogo apenas com alteração do estado do pacman e dos fantasmas caso o pacman esteja __Mega__ e o novo estado do(s) fantasma(s) após ter(em) sido comido(s)
*Retorna o novo estado do jogo com o estado do(s) fantasma(s) comido(s) atualizado na lista de jogadores

-}

-- | Função para jogada com o Pacman
--
pacPlay :: Play -> State -> State
pacPlay (Move j o) (State (x:xs) (p:ps) l) = case coordsFood (x:xs) of
                                                  [] -> error "You win!"
                                                  _  -> if isthereGhost (Move j o) (x:xs) (p:ps) == True then newStateGhost
                                                        else newState (State (x:xs) (p:ps) l) player
    where player = openOrClosed $ movePlay (Move j o) (x:xs) (p:ps)
          ghostNew = stateGhost (x:xs) (findPlayer j (p:ps)) (findPlayerscoord (p:ps) coord) 
          newStateGhost = updatePlayersGhosts (newState (State (x:xs) (p:ps) l) player) ghostNew
          coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))

-- | Função final play
--
play :: Play -> State -> State
play (Move j o) (State (x:xs) (p:ps) l) = case isitGhost (findPlayer j (p:ps)) of 
                                                True -> playGhost (Move j o) (State (x:xs) (p:ps) l)
                                                False -> pacPlay (Move j o) (State (x:xs) (p:ps) l)

-- | Função para jogada com o Fantasma
--
playGhost :: Play -> State -> State
playGhost (Move j o) (State (x:xs) (p:ps) l) = if istherePac (Move j o) (x:xs) (p:ps) == True then newStateFinal else newStateGhost 
          where ghost = movePlayGhost (Move j o) (x:xs) (p:ps)
                newStateGhost = updateState (State (x:xs) (p:ps) l) ghost 
                pacman = collidePac (getPacman (p:ps))
                newStateFinal = updateState newStateGhost pacman

-- | Função que dada uma jogada, um labirinto e uma lista de jogadores, devolve o jogador de ID j atualizado
--
movePlayGhost :: Play -> Maze -> [Player] -> Player
movePlayGhost (Move j o) (x:xs) (p:ps) = if o == getPlayerOrientation (findPlayer j (p:ps)) then  case o of 
                                                                                               R -> moveAfterPacSide (Move j o) (x:xs) (p:ps) 1 R
                                                                                               L -> moveAfterPacSide (Move j o) (x:xs) (p:ps) (-1) L
                                                                                               U -> moveAfterPacVertical (Move j o) (x:xs) (p:ps) (-1) U
                                                                                               D -> moveAfterPacVertical (Move j o) (x:xs) (p:ps) 1 D
                                    else changeOrientation (Move j o) (x:xs) (findPlayer j (p:ps)) o

-- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação ao ghost e às orientações para __esquerda__ e __direita__
--
moveAfterPacSide:: Play -> Maze ->[Player] -> Int -> Orientation -> Player
moveAfterPacSide (Move j o) (x:xs) (p:ps) i ori = if istherePacman (p:ps) (a,b) == True then 
                                                               if playerMode == Mega then ghostHome' (x:xs) (findPlayer j (p:ps))
                                                               else movePlayerSide (Move j o) (x:xs) (findPlayer j (p:ps)) i ori --VERIFICAR
                                                    else movePlayerSide (Move j o) (x:xs) (findPlayer j (p:ps)) i ori 
                                                        where (a,b) = coordB (getCoords (findPlayer j (p:ps))) i
                                                              playerMode = pacMode (getPacman (findPlayerscoord (p:ps) (a,b)))

-- | Função que dado uma jogada, um labirinto, um jogador, um inteiro e uma orientação, devolve o jogador após a jogada. Esta função é em relação ao ghost e às orientações para __cima__ e __baixo__
--
moveAfterPacVertical:: Play -> Maze ->[Player] -> Int -> Orientation -> Player
moveAfterPacVertical (Move j o) (x:xs) (p:ps) i ori = if istherePacman (p:ps) (a,b) == True then 
                                                               if playerMode == Mega then ghostHome' (x:xs) (findPlayer j (p:ps))
                                                               else movePlayerVertical (Move j o) (x:xs) (findPlayer j (p:ps)) i ori --VERIFICAR
                                                    else movePlayerVertical (Move j o) (x:xs) (findPlayer j (p:ps)) i ori 
                                                        where (a,b) = coordA (getCoords (findPlayer j (p:ps))) i
                                                              playerMode = pacMode (getPacman (findPlayerscoord (p:ps) (a,b)))

-- | Função que confirma se, nas coordenadas onde o jogador pretende ir existe ou não um pacman
--
istherePacman :: [Player] -> Coords -> Bool
istherePacman [] _ = False
istherePacman (p:ps) (a,b)
  |istherePlayer (p:ps) (a,b) = if isitGhost (findPlayerCoord (p:ps) (a,b)) == True then False else True 
  |otherwise = False

-- | Função que retira da lista de jogadores o player que corresponde ao pacman
--
getPacman :: [Player] -> Player 
getPacman (p:ps) = if isitGhost p == False then p else getPacman ps 

-- | Função que move um fantasma comido de volta para a casa dos fantasmas e volta a estar vivo
--
ghostHome' :: Maze -> Player -> Player
ghostHome' (x:xs) (Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)) = if even (length (x:xs)) then (Ghost (GhoState (id, (middleMaze (x:xs),middleWidth (x:xs)), v , o, points, lifes) Alive)) else (Ghost (GhoState (id, ((middleMaze(x:xs) +1),middleWidth (x:xs)), v, o, points, lifes) Alive)) 

-- | Função que quando o pacman colide com um fantasma, decide que, caso o estado do pacman seja Mega, este come o fantasma e ganha 10 pontos, caso contrário, se estiver em estado normal, perde uma vida. Se tiver 0 vidas altera o seu estado para Dying.
--
collidePac :: Player -> Player 
collidePac (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))  = case pacmanMode of 
                                                                                                           Normal -> if lifes == 0 then (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed Dying)) else (Pacman (PacState (id, (a,b), v, o, points, lifes -1) timeMega openClosed pacmanMode)) 
                                                                                                           Mega -> (Pacman (PacState (id, (a,b), v, o, points+10, lifes) timeMega openClosed pacmanMode)) 
                                                                                                           Dying -> error "Game Over"
-- | Função que define a posição da boca do pacman                                                         
--
openOrClosed :: Player -> Player
openOrClosed (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) = case openClosed of 
                                                                                                     Open -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega Closed pacmanMode))
                                                                                                     Closed -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega Open pacmanMode))


-- | Função que dado um labirinto, retorna a lista com coordenadas de comidas grandes e pequenas 
--
coordsFood :: Maze -> [Coords]
coordsFood maze = foldl (\acc y -> acc ++ (foldl (\acc2 x -> if ((maze !! y) !! x == Food Little) || ((maze !! y) !! x == Food Big) then acc2 ++ [(y,x)] else acc2)) [] [0..(length (head maze) - 1)]) [] [0..(length maze - 1)]
