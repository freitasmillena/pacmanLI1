{-|
Module: Tarefa5
Description: Módulo Haskell que implementa um comportamento para os fantasmas
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>

Um módulo Haskell que implementa um comportamento para os fantasmas para que fiquem inteligentes e escolham quais estratégias a seguir consuante o estado do jogo.


=Introdução

A tarefa 5 foi a responsável por implementar o comportamento inteligente para os fantasmas. A função objetivo desta tarefa __ghostPlay__ é necessária para a realização também da Tarefa 4.


=Objetivos

Nosso principal objetivo era que os fantasmas pudessem perseguir ou fugir do Pacman de acordo com seu estado no jogo, para atribuir emoção e dificuldade para o usuário, mas de uma forma que não tornasse o jogo totalmente impossível. Para que também não se tornasse muito fácil, tivemos como objetivo encontrar uma forma com que os fantasmas pudessem ter personalidades diferentes que influenciariam suas escolhas para fugir ou perseguir o Pacman.


=Discussão e conclusão

Decidimos implementar personalidades diferentes. A princípio, pensamos em alterar o ficheiro Types que contém o estado completo dos fantasmas e adicionar a personalidade para que fosse utilizada de modo a escolher a estratégia. Porém, acabamos por optar por uma solução mais simples e com maior probabilidade de eficiência. Separamos os fantasmas por id par e ímpar. Os de id par sempre tentam alcançar ou fugir da coordenada atual do Pacman e, os de id ímpar tentam prever em qual coordenada o Pacman irá se encontrar após 4 jogadas, de acordo com a orientação atual, e alcançar ou fugir desta.

Começamos por definir como saberíamos a distância entre o fantasma e o Pacman. Decidimos diminuir suas coordenadas e, desta forma, conseguimos obter o número de jogadas necessário para que o fantasma chegue até o Pacman. Isto foi utilizado para fugir e perseguir, a primeira ao escolher de forma com que fosse necessário o maior número de jogadas e a segunda ao escolher o menor.

Chegamos num impasse em como determinar a orientação do fantasma para a próxima jogada. Chegamos à conclusão de que seria mais simples observar ao redor do fantasma para onde ele poderia ir. Os jogadores podem se movimentar verticalmente e horizontalmente. Utilizamos isto e depois filtramos para selecionar apenas as coordenadas que realmente alterariam a posição do fantasma, ou seja, as que não possuem __Wall__. Desta forma, com estas coordenadas fomos capazes de realizar o que foi explicado anteriormente: calcular a diferença entre essas coordenadas e a coordenada do Pacman (atual ou prevista) e escolher a que tivesse menor ou maior números de jogadas.

Encontramos uma dificuldade que, como escolhemos a funções __maximum__ e __minimum__, estas comparavam os primeiros elementos dos tuplos com as jogadas para determinar o menor ou maior. Isto não era eficaz, pois um tuplo (0,3) -/3 jogadas/- seria dado como o menor mesmo se houvesse um (1,0) -/1 jogada/-. Portanto, lembramos de como podemos calcular a distância à origem e adaptamos ao cenário que temos. Desta forma, resultou conforme queríamos.

Quando um fantasma morre, no nosso jogo, volta exatamente para o centro da casa de fantasmas. Portanto, tivemos que implementar a condição na qual o fantasma encontra-se nesta coordenada. Assim, tendo que usar a próxima orientação para cima a fim de sair da casa de fantasmas.

Em conclusão, ficamos contentes com o comportamento dos nossos fantasmas. Esta tarefa foi muito interessante de realizar, pois nos fez realmente pensar por muitos dias, conversar muito e pesquisar muito para elaborar raciocínios e estratégias que fizessem sentido e fazer com que funcionassem. A princípio, queríamos partir pela ideia do caminho mais curto ao aplicar o algoritmo de Dijkstra ou A*, porém ao pesquisar percebemos que isto iria precisar de mais tempo e talvez não fosse a melhor solução por questões de tornar o jogo mais pesado ou menos fluido. 



-}

module Tarefa5 where 

import Types
import Tarefa2
import Tarefa1

-- | Função que dado uma coordenada e outra coordenada, calcula a diferença entre elas
--
coordDif :: Coords -> Coords -> Coords
coordDif (x,y) (a,b) = (a-x,b-y)


-- | Função que dado uma coordenada e uma lista de corredores, ou seja, um labirinto, diz se há ou não __Wall__ nesta coordenada
--
isitWall :: Coords -> [Corridor] -> Bool
isitWall _ [] = False
isitWall (a,b) (x:xs) = idpieceCoord (x:xs) (a,b) == Wall

-- | Função que recebe lista de coordenadas em volta do fantasma e determina as que ele pode ir, ou seja, que não há parede 
--
notWall :: [Coords] -> [Corridor] -> [Coords]
notWall [] _ = []
notWall ((a,b):bs) (x:xs)
    |isitWall (a,b) (x:xs) = notWall bs (x:xs)
    |otherwise = (a,b) : notWall bs (x:xs)

-- | Função que dado uma coordenada e uma orientação, assume que a orientação permanecerá a mesma pelas próximas 4 jogadas e determina a coordenada final
--
nextCoordP :: Coords -> Orientation -> Coords 
nextCoordP (a,b) o = case o of 
                       R -> (a,b+4)
                       L -> (a,b-4)
                       U -> (a-4,b)
                       D -> (a+4,b)

-- | Função que passa as coordenadas para seu valor absoluto
--
absList :: [Coords] -> [Coords]
absList [] = []
absList ((a,b):bs) = ((abs a),(abs b)) : absList bs 

-- | Função que calcula a distância da coordenada à origem
--
square :: Coords -> Float  
square (a,b) = sqrt $ fromIntegral $ (a^2 + b^2)

-- | Função que dado um número e uma lista, determina o índice deste número na lista
index' :: Float -> [Float] -> Int 
index' n (x:xs) 
   |n == x = 0
   |otherwise = (index' n xs) + 1 

-- | Função que recebe a coordenada atual do fantasma e a que ele quer ir e devolve a orientação necessária para a próxima jogada. Considera-se apenas que a coordenada desejada difere apenas de 1.
--
whichOrientation :: Coords -> Coords -> Orientation
whichOrientation (a,b) (c,d) 
    |c == a+1 = D
    |c == a-1 = U
    |d == b+1 = R 
    |d == b-1 = L 
    |otherwise = Null  

{- | Função que recebe a lista de coordenadas possíveis para o fantasma, a coordenada do pacman e devolve a coordenada melhor para o fantasma ir de forma que se encontre mais distante do pacman 

@
lc = map (coordDif (x,y)) ((a,b):bs)
@
*((a,b):bs) corresponde a lista das coordenadas possíveis que o fantasma pode ir;
*(x,y) corresponde a coordenada que ele pretende estar mais distante;
*Com isto, aplica-se a função __coordDif__ a todas as coordenadas da lista, pela qual irá resultar na lista das diferenças entre cada coordenada e a coordenada (x,y);
*Estas diferenças determinarão o número de jogadas necessárias para chegar na coordenada (x,y).


@
best = maximum $ map (square) $ absList lc
@
*Caso o fantasma precisasse ir para a esquerda ou para cima, teríamos tuplos com as jogadas necessárias nos quais os elementos seriam negativas. Portanto, para que o efeito de comparação da que contém menos jogadas o possível seja válido, optamos por utilizar o valor absoluto dos elementos desta lista;
*Caso tivéssemos [(0,3),(1,0)] e aplicássemos o maximum, este resultaria em (1,0). Neste caso, não seria válido pois corresponde a 1 jogada enquanto (0,3) corresponde a 3 jogadas. Portanto, não retornaria o tuplo com o maior número de jogadas possível;
*Para resolver este problema, optamos por chamar a função __square__ na qual calcula a distância a origem da coordenada, agora, ao chamar o maximum, devolve o que era suposto;
*Portanto, best corresponde ao tuplo que contém o maior número de jogadas possível para chegar na coordenada desejada.

@
n2 = index' best (map (square) $ absList lc)
@
*Agora, precisamos saber qual coordenada das possíveis que o fantasma pode se locomover é que este tuplo de jogadas corresponde;
*Para isto, achamos o índice do tuplo na lista de tuplos. 

@
((a,b):bs) !! n2 
@
*Como a lista encontra-se na mesma ordem que as coordenadas possíveis do fantasma, conseguimos saber qual coordenada corresponde a este índice;
*Portanto, esta é a coordenada que o fantasma deve escolher ir.
-}
betterRun :: [Coords] -> Coords -> Coords
betterRun ((a,b):bs) (x,y) = ((a,b):bs) !! n2 
   where best = maximum $ map (square) $ absList lc
         lc = map (coordDif (x,y)) ((a,b):bs)
         n2 = index' best (map (square) $ absList lc)


{- | Função que recebe o fantasma, as coordenadas do pacman no mapa, o labirinto e devolve a orientação para a próxima jogada do fantasma para que o fantasma se distancie do pacman

@
lc = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
@
*Estas são as coordenadas para qual o fantasma tem a opção de se locomover

@
poss = notWall lc (x:xs)
@
*Representam as coordenadas que o fantasma pode ir nas quais não há __Wall__

@
theone = betterRun poss (c,d)
@
*É aplicada a função __betterRun__ que irá comparar as coordenadas possíveis para o fantasma com a coordenada recebida do Pacman (podendo ser a atual ou a prevista após 4 jogadas) e irá determinar a que o levará a estar mais distante do Pacman

@
whichOrientation (a,b) theone
@
*Esta função recebe a coordenada atual do fantasma e determina qual __Orientation__ este deve escolher na Play para que se distancie do Pacman.
-}
littleRunaway :: Player -> Coords -> [Corridor] -> Orientation 
littleRunaway (Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)) (c,d) (x:xs) = whichOrientation (a,b) theone
    where poss = notWall lc (x:xs)
          theone = betterRun poss (c,d)
          lc = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]


{- | Função que determina a melhor jogada caso o fantasma se encontre __Dead__ com o objetivo de fugir do Pacman


@
if (z,w) == (middleMaze (x:xs),middleWidth (x:xs)) then Move n U
@
*Caso a coordenada atual do fantasma corresponda ao centro da casa de fantasmas, ele irá movimentar-se para cima a fim de sair da casa de fantasmas.

@
ghost = findPlayer n (p:ps)
@
*Para selecionar o fantasma com id __n__ da lista de jogadores

@
(z,w) = getPlayerCoords ghost
@
*Para obter a coordenada atual deste fantasma

@
coordp = getPlayerCoords $ getPacman (p:ps)
@
*Selecionamos o pacman e obtemos sua coordenada atual

@
pacOri = getPlayerOrientation $ getPacman (p:ps)
@
*Obtemos a orientação atual do pacman

@
coordp2 = nextCoordP coordp pacOri 
@
*De acordo com a orientação do pacman, obtemos a coordenada equivalente a 4 casas a frente;
*Por exemplo, caso o pacman tenha coordenada (a,b) e esteja com orientação R, teremos (a,b+4). É uma forma de prever para onde o Pacman pretende seguir.

= Decidimos dividir em duas estratégias consuante o id do fantasma:

==/ID par/
===Caso o ID seja par, o fantasma irá optar pela __Orientation__ pela qual sua pŕoxima jogada o levará para a coordenada mais distante da coordenada atual do Pacman

@
ori = littleRunaway ghost coordp (x:xs)
@
*É aplicada a função __littleRunaway__ que contém estratégia para que o fantasma fuja da coordenada atual do Pacman.

==/ID ímpar/

===Caso o ID seja ímpar, o fantasma tentará prever em qual coordenada o Pacman estará após quatro jogados e irá optar pela __Orientation__ pela qual sua próxima jogada o levará para a coordenada mais distante da coordenada do Pacman após quatro jogadas.

@
o = littleRunaway ghost coordp2 (x:xs)
@
*É aplicada a função __littleRunaway__ que contém estratégia para que o fantasma fuja da coordenada prevista do Pacman após quatro jogadas.
-}
scatterMode :: State -> Int -> Play
scatterMode (State (x:xs) (p:ps) l) n = if (z,w) == (middleMaze (x:xs),middleWidth (x:xs)) then Move n U else case (even n) of 
                                                                                                                  True -> Move n ori 
                                                                                                                  False -> Move n o 
    where ori = littleRunaway ghost coordp (x:xs)
          o = littleRunaway ghost coordp2 (x:xs)
          ghost = findPlayer n (p:ps)
          coordp = getPlayerCoords $ getPacman (p:ps)
          pacOri = getPlayerOrientation $ getPacman (p:ps)
          coordp2 = nextCoordP coordp pacOri 
          (z,w) = getPlayerCoords ghost



{- | Função que recebe a lista de coordenadas possíveis para o fantasma, a coordenada do pacman e devolve a coordenada melhor para o fantasma ir de forma que se encontre mais perto do pacman 

@
lc = map (coordDif (x,y)) ((a,b):bs)
@
*((a,b):bs) corresponde a lista das coordenadas possíveis que o fantasma pode ir;
*(x,y) corresponde a coordenada que ele pretende estar mais próximo;
*Com isto, aplica-se a função __coordDif__ a todas as coordenadas da lista, pela qual irá resultar na lista das diferenças entre cada coordenada e a coordenada (x,y);
*Estas diferenças determinarão o número de jogadas necessárias para chegar na coordenada (x,y).


@
best = minimum $ map (square) $ absList lc
@
*Caso o fantasma precisasse ir para a esquerda ou para cima, teríamos tuplos com as jogadas necessárias nos quais os elementos seriam negativas. Portanto, para que o efeito de comparação da que contém menos jogadas o possível seja válido, optamos por utilizar o valor absoluto dos elementos desta lista;
*Caso tivéssemos [(0,3),(1,0)] e aplicássemos o __minimum__, este resultaria em (0,3). Neste caso, não seria válido pois corresponde a 3 jogadas enquanto (1,0) corresponde a 1 jogada. Portanto, não retornaria o tuplo com o menor número de jogadas possível;
*Para resolver este problema, optamos por chamar a função __square__ na qual calcula a distância a origem da coordenada, agora, ao chamar o minimum, devolve o que era suposto;
*Portanto, best corresponde ao tuplo que contém o menor número de jogadas possível para chegar na coordenada desejada.

@
n2 = index' best (map (square) $ absList lc)
@
*Agora, precisamos saber qual coordenada das possíveis que o fantasma pode se locomover é que este tuplo de jogadas corresponde;
*Para isto, achamos o índice do tuplo na lista de tuplos. 

@
((a,b):bs) !! n2 
@
*Como a lista encontra-se na mesma ordem que as coordenadas possíveis do fantasma, conseguimos saber qual coordenada corresponde a este índice;
*Portanto, esta é a coordenada que o fantasma deve escolher ir.
-}
bestCoord :: [Coords] -> Coords -> Coords
bestCoord ((a,b):bs) (x,y) = ((a,b):bs) !! n2 
   where best = minimum $ map (square) $ absList lc
         lc = map (coordDif (x,y)) ((a,b):bs)
         n2 = index' best (map (square) $ absList lc)

{- | Função que recebe o fantasma, as coordenadas do pacman no mapa, o labirinto e devolve a orientação para a próxima jogada do fantasma para que o fantasma se aproxime do pacman

@
lc = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
@
*Estas são as coordenadas para qual o fantasma tem a opção de se locomover

@
poss = notWall lc (x:xs)
@
*Representam as coordenadas que o fantasma pode ir nas quais não há __Wall__

@
theone = bestCoord poss (c,d)
@
*É aplicada a função __bestCoord__ que irá comparar as coordenadas possíveis para o fantasma com a coordenada do Pacman (podendo ser a atual ou a prevista após 4 jogadas) e irá determinar a que o levará a estar mais próximo do Pacman

@
whichOrientation (a,b) theone
@
*Esta função recebe a coordenada atual do fantasma e determina qual __Orientation__ este deve escolher na Play para que se aproxime do Pacman.
-}
goToPac :: Player -> Coords -> [Corridor] -> Orientation 
goToPac (Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)) (c,d) (x:xs) = whichOrientation (a,b) theone
    where poss = notWall lc (x:xs)
          theone = bestCoord poss (c,d)
          lc = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]


{- | Função que determina a melhor jogada caso o fantasma se encontre __Alive__ com o objetivo de perseguir o Pacman


@
if (z,w) == (middleMaze (x:xs),middleWidth (x:xs)) then Move n U
@
*Caso a coordenada atual do fantasma corresponda ao centro da casa de fantasmas, ele irá movimentar-se para cima a fim de sair da casa de fantasmas.

@
ghost = findPlayer n (p:ps)
@
*Para selecionar o fantasma com id __n__ da lista de jogadores

@
(z,w) = getPlayerCoords ghost
@
*Para obter a coordenada atual deste fantasma

@
coordp = getPlayerCoords $ getPacman (p:ps)
@
*Selecionamos o pacman e obtemos sua coordenada atual

@
pacOri = getPlayerOrientation $ getPacman (p:ps)
@
*Obtemos a orientação atual do pacman

@
coordp2 = nextCoordP coordp pacOri 
@
*De acordo com a orientação do pacman, obtemos a coordenada equivalente a 4 casas a frente;
*Por exemplo, caso o pacman tenha coordenada (a,b) e esteja com orientação R, teremos (a,b+4). É uma forma de prever para onde o Pacman pretende seguir.

= Decidimos dividir em duas estratégias consuante o id do fantasma:

==/ID par/
===Caso o ID seja par, o fantasma irá optar pela __Orientation__ pela qual sua pŕoxima jogada o levará para a coordenada mais próxima da coordenada atual do Pacman

@
ori = goToPac ghost coordp (x:xs)
@
*É aplicada a função __goToPac__ que contém estratégia para que o fantasma vá para a coordenada atual do Pacman.

==/ID ímpar/

===Caso o ID seja ímpar, o fantasma tentará prever em qual coordenada o Pacman estará após quatro jogados e irá optar pela __Orientation__ pela qual sua próxima jogada o levará para a coordenada mais próxima da coordenada do Pacman após quatro jogadas.

@
o = goToPac ghost coordp2 (x:xs)
@
*É aplicada a função __goToPac__ que contém estratégia para que o fantasma siga para a coordenada prevista do Pacman após quatro jogadas.
-}
chaseMode :: State -> Int -> Play
chaseMode (State (x:xs) (p:ps) l) n = if (z,w) == (middleMaze (x:xs),middleWidth (x:xs)) then Move n U else case (even n) of 
                                                                                                                     True -> Move n ori 
                                                                                                                     False -> Move n o 
    where ori = goToPac ghost coordp (x:xs)
          o = goToPac ghost coordp2 (x:xs)
          ghost = findPlayer n (p:ps)
          coordp = getPlayerCoords $ getPacman (p:ps)
          pacOri = getPlayerOrientation $ getPacman (p:ps)
          coordp2 = nextCoordP coordp pacOri 
          (z,w) = getPlayerCoords ghost


{- | Função que dado uma lista de fantasmas e o estado atual do jogo, devolve a lista de jogadas com a melhor estratégia

@
|ghostMode == Alive = chaseMode st id : runOrChase gs st
@
*Caso o primeiro fantasma da lista seja __Alive__, a função __chaseMode__ é chamada para este fantasma de modo com que persiga o pacman. E __runOrChase__ é chamada recursivamente para os restantes fantasmas.

@
|otherwise = scatterMode st id : runOrChase gs st      
@
*Caso o primeiro fantasma da lista seja __Dead__, a função __scatterMode__ é chamada para este fantasma de modo com que fuja do pacman. E __runOrChase__ é chamada recursivamente para os restantes fantasmas.

-}
runOrChase :: [Player] -> State -> [Play]
runOrChase [] _ = []
runOrChase ((Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)):gs) st@(State (x:xs) (p:ps) l) 
   |ghostMode == Alive = chaseMode st id : runOrChase gs st
   |otherwise = scatterMode st id : runOrChase gs st                                                      


{- | Função que dado o estado atual do jogo, devolve a lista de jogadas com a melhor estratégia para os fantasmas

@
runOrChase (g:gs) st
@
*Aplica-se a função auxiliar __runOrChase__ a lista de jogadores fantasmas. Esta função é explicada mais a frente e é responsável pelos fantasmas optarem pela melhor estratégia caso seu estado seja __Alive__ ou __Dead__.

-}
ghostPlay :: State -> [Play]
ghostPlay st@(State (x:xs) (p:ps) l) = runOrChase (g:gs) st 
    where (g:gs) = getGhosts (p:ps)

{- | Função que dado o estado atual do jogo, devolve a lista de jogadas com a melhor estratégia para os fantasmas que encontram-se __Alive__

===Foi necessário criar esta função que ser aplicada na Tarefa 4, na qual quando o step não é múltiplo de 2, apenas os fantasmas sem a velocidade reduzida efetuam jogadas

@
where (g:gs) = getGhosts' $ getGhosts (p:ps)
@
*Primeiro selecionamos os fantasmas da lista de jogadores e, a seguir, selecionamos apenas os fantasmas __Alive__

===De resto, a função comporta-se da mesma maneira que a __ghostPlay__ 

-}
ghostPlay2 :: State -> [Play]
ghostPlay2 st@(State (x:xs) (p:ps) l) = runOrChase (g:gs) st 
     where (g:gs) = getGhosts' $ getGhosts (p:ps)


-- | Função que dado uma lista de fantasmas, retorna apenas a lista com os fantasmas que encontram-se __Alive__
--
getGhosts' :: [Player] -> [Player]
getGhosts' [] = []
getGhosts' ((Ghost (GhoState (id, (a,b), v, o, points, lifes) Alive)):gs) = (Ghost (GhoState (id, (a,b), v, o, points, lifes) Alive)) : getGhosts' gs 
getGhosts' ((Ghost (GhoState (id, (a,b), v, o, points, lifes) Dead)):gs) = getGhosts' gs 


--TODO Relatório 