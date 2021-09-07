{-|
Module: Tarefa4
Description: Módulo Haskell que implementa a passagem de tempo no jogo Pacman
Copyright: Maria Pinheiro <mcardosorp@gmail.com>;
           Millena Santos <freitasmillena27@gmail.com>

Um módulo Haskell que calcula o efeito da passagem de um instante de tempo num estado do jogo.

= Introdução

A tarefa 4 foi a responsável por introduzir a passagem de tempo ao jogo e fazer com que todos os jogadores efetuassem jogadas ao passar do tempo. Foi a partir desta tarefa que pudemos sentir realmente o jogo e visualizar o trabalho feito até então, pois antes era estático e agora somos capazes de ver as mudanças.

= Objetivos

O nossos principais objetivos era fazer o Pacman fosse realmente afetado pela passagem de tempo. Ou seja, caso estivesse Mega, realmente tivesse seu tempo Mega a ser diminuído até que voltasse a se encontrar em estado Normal e também que os Fantasmas pudessem reagir à passagem de tempo ao efetuar jogadas.

No caso do Pacman, da mesma forma que o jogo original, enquanto uma tecla não fosse carregada para determinar a próxima orientação, este permaneceria a efetuar jogadas na orientação que se encontra até que o usuário determinasse o contrário.

Os fantasmas efetuariam jogadas de acordo com a função __ghostPlay__ implementada na Tarefa 5 de modo com que fossem inteligentes para perseguir o Pacman caso estivessem Alive ou fugir do Pacman caso estivessem Dead. Ou seja, trazer mais dificuldade para o jogo e maior interação e emoção para o usuário.

= Discussão e Conclusão

Decidimos que seria mais prático se dividíssemos o passTime para Pacman e para fantasmas.

===/Pacman/

Como nosso tempo escolhido para o Pacman permanecer __Mega__ foi de 10 segundos e o defaultDelayTime é de 250ms, chegamos à conclusão de que o tempo Mega deveria diminuir 1 segundo a cada 4 steps. Por isto, separamos a condição na qual o step é múltiplo de 4 e o Pacman está Mega.

Para que o o Pacman pudesse ter seu estado atualizado com a passagem do tempo, resolvemos modificar o Pacman recebido no estado atual ao utilizar as funções que fazem com que seu tempo Mega seja diminuído em 1 e que volte ao Normal caso alcance 0. 

Atualizamos o estado do jogo recebido de modo a substituir o Pacman anterior pelo atual e chamamos a função __play__ da tarefa 2 para o Pacman de modo que permaneça na mesma orientação e que utilize o Pacman já atualizado. Assim, conseguimos evitar que o tempo Mega ficasse negativo, que o Pacman não voltasse  a Normal e, consequentemente, os fantasmas a Alive e que a passagem de tempo funcionasse conforme o esperado.


===/Fantasmas/

Nesta etapa, começamos pelo raciocínio de que a velocidade no jogo seria 1. Quando os fantasmas encontram-se __Dead__, suas velocidades são reduzidas pela metade, pelo qual só efetuariam jogadas de 2 em 2 steps.

Estamos conscientes de que num jogo com passagens de níveis e maiores dificuldades, haveria uma implementação de velocidades diferentes. Infelizmente, por questões de logística, não tivemos a oportunidade de implementar isto desta vez. Portanto, temos apenas duas velocidades possíveis: 1.0 e 0.5.

Portanto, separamos a passTime para os fantasmas para quando o step fosse múltiplo de 2, na qual todos os fantasmas efetuam jogadas e, caso não seja, apenas os __Alive__ que possuem velocidade 1.0 que efetuam jogadas.

Encontramos a dificuldade em como aplicar isto na função de modo que todos os fantasmas efetuassem jogadas e que obtivéssemos um estado final do jogo apenas após todos os fantasmas jogarem. Pois esta recebe um estado do jogo e, desta vez, não temos apenas um jogador como para o Pacman, mas temos vários fantasmas. 

Como usamos a ghostPlay para que os fantasmas fossem inteligentes, temos uma lista de jogadas. Sendo cada elemento da lista a jogada para um fantasma com um id específico.

Optamos por fazer com que o estado do jogo acumulasse até que devolvesse o estado final. Ou seja, o primeiro fantasma faz sua jogada utilizando o estado recebido pela função. Esta jogada irá devolver um novo estado que será utilizado para a jogada do próximo fantasma e, assim, sucessivamente até que todos os fantasmas tivessem jogado e devolvesse o estado final.

Isto nos pareceu uma boa resolução, pois nos ajudou a resolver outra dificuldade que tínhamos, que era como juntar o estado gerado pela passTime para o Pacman com os gerados pelos fantasmas. Portanto, na função passTime final, utilizamos o estado do jogo após a jogada do Pacman como o estado do jogo recebido pela passTime dos fantasmas. Com isto, o estado devolvido contém todas as jogadas efetuadas.

Ficamos muito contentes com o resultado final. Esta provavelmente foi a tarefa que achamos mais divertida e que gostamos mais de visualizar o resultado ao terminá-la. Isto porque pudemos sentir a nostalgia ao finalmente poder ver o jogo a funcionar como o Pacman original e pudemos testar e jogar desta vez dinamicamente apenas ao carregar em teclas. Outro motivo pelo qual gostamos tanto desta tarefa foi porque fez com que voltássemos à Tarefa 2 e, com isto, conseguimos consertar erros que haviam passados despercebidos e optimizar alguns pormenores. 


-}



module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5 


defaultDelayTime = 250 -- 250 ms

{- | Função que recebe um int, equivalente a um step, e o estado atual do jogo, e que devolve um state no qual aplica a ação de passar o tempo. Esta função é __geral__ ou seja, para __todos os jogadores__ 

@
newSt = passTimePac n st 
@
*O estado original é atualizado para o _newSt_, cujo corresponde à atualização do estado original consuante a aplicação da passagem do tempo ao pacman no step n.

@
passTimeGhosts n newSt 
@
* A função aplica a função passTimeGhosts a um step n e a um estado newSt, a que vai aplicar a passagem do tempo aos fantasmas ao novo estado (atualizado com a passagem do tempo do pacman), criando, assim o estado final onde foi aplicada a passagem do tempo a todos os jogadores. 
-}
passTime :: Int -> State -> State 
passTime n st = passTimeGhosts n newSt 
  where newSt = passTimePac n st 

{- | Função que recebe um step e um state, e que devolve um state no qual aplica a ação de passar o tempo. Esta função é espeficiamente para os _fantasmas_

@
plays = ghostPlay st 
@
*Aplica a função ghostPlay e retorna a lista de jogadas para todos os fantasmas da lista de jogadores.

@
alives = ghostPlay2 st 
@
*Aplica a função ghostPlay2 e retorna a lista de jogadas para todos os fantasmas __Alive__ da lista de jogadores

==/Step múltiplo de 2/
===Tornou-ne necessário dividir em múltiplos de 2 e não múltiplos, pois, considera-se que a velocidade normal dos fantasmas é __1__, porém quando estão __Dead__ encontram-se com a velocidade reduzida pela metade. Como o defaultDelayTime é de 250ms, um jogador com velocidade __0.5__ irá efetuar jogadas apenas de 2 em 2 steps.

@
mod n 2 == 0 = foldl (\acc x -> play x acc) st plays
@
*Verifica-se inicialmente se o step é ou não múltiplo de 2;
*Com isto, aplica-se um foldl cujo acumulador é o state recebido na função passTimeGhosts e a lista inicial corresponde a lista de jogadas para todos os fantasmas, já que o step é múltiplo de 2;
*Basicamente, irá chamar a função play para a primeira jogada da lista de jogadas e o state utilizado será o recebido. Isto irá gerar um novo state. Este novo state será utilizado quando chamar novamente a função play para a segunda jogada da lista de jogadas e, assim, sucessivamente até que percorra toda a lista de jogadas e devolva o state final no qual todos os fantasmas efetuaruam suas respetivas jogadas.

==/Step não múltiplo de 2/

@
otherwise = foldl (\acc x -> play x acc) st alives
@
*Neste caso, apenas os fantasmas __Alive__, ou seja, com velocidade __1__ que efetuam jogadas;
*O foldl funciona da mesma maneira que acima. Inicia chamando a função Play para a primeira jogada e utiliza o state recebido e vai acumulando novos states para serem aplicados ao percorrer a lista até que devolva o state final;
*A diferença é que, neste caso, a lista de jogadas a ser percorrida contém jogadas apenas para os fantasmas com velocidade __1__.

-}
passTimeGhosts :: Int -> State -> State 
passTimeGhosts n st
   |mod n 2 == 0 = foldl (\acc x -> play x acc) st plays
   |otherwise = foldl (\acc x -> play x acc) st alives
   where plays = ghostPlay st 
         alives = ghostPlay2 st 
         
{- | Função que recebe um int, equivalente a um step e um state, e que devolve um state no qual aplica a ação de passar o tempo. Esta função é espeficiamente para o _pacman_

@
j = getPlayerID $ getPacman (p:ps)
@
*Através desta expressão conseguimos extrair o player que corresponde ao pacman da lista de jogadores recebida no state, e após isto, conseguimos recolher o id do pacman que corresponderá à letra j.

@
o = getPlayerOrientation $ getPacman (p:ps)
@
*Através desta expressão conseguimos extrair o player que corresponde ao pacman da lista de jogadores recebida no state, e após isto, conseguimos retirar a orientação do pacman.

@
player = (backToNormal $ timeMegaCD $ getPacman $ getPlayers st )
@
*Através desta expressão obtemos um player. Inicialmente extraimos a lista de jogadores do State recorrendo à função getPlayers. Após isto, recolhemos o player que corresponde ao pacman através da função getPacman. Por fim, aplicamos por esta ordem, a função timeMegaCD e a função backToNormal, que irão alterar o timeMega e o pacMode, se necessário. O resultado destas ações será o player.

@
ns = newState st player
@
*Através desta expressão altera-se o State original, utilizando a função newState definida na tarefa 2. A função recebe o State original e um argumento player, que seria um Player atualizado consuante as seguintes condições: 
     >Caso esteja Mega, o timeMega diminui 1;
     >Caso acabe o timeMega, retorna a Normal.

==/Pacman Mega e step múltiplo de 4/
===Escolhemos step múltiplo de 4 pois optamos pelo timeMega de 10 segundos. Como o defaultDelayTime corresponde a 250 ms, isto significa que o timeMega irá diminuir em 1 a cada 4 jogadas, ou seja, de 4 em 4 steps.

@
pacMode (getPacman (p:ps)) == Mega && mod n 4 == 0 = play (Move j o) ns
@
*Verifica-se inicialmente se o pacman está em estado Mega;
*Se estiver em estado mega verifica-se se o step, que corresponde ao n, é múltiplo de 4;
*Se assim acontecer, o state devolvido resultará da aplicação da função play no pacman. Nesta última, utiliza-se uma jogada (Move j o), onde j seria o id do pacman e o a orientação original do pacman; e utiliza-se um State ns que seria o state atualizado consuante as condições anteriormente descritas.

==/Pacman diferente de Mega e step não múltiplo de 4/
@
otherwise = play (Move j o) st
@
*Caso o pacman não esteja em estado Mega __ou__ caso o step não seja múltiplo de 4, __ou ambos__, o State devolvido resultará da aplicação da função play;
*Nesta última utiliza-se uma jogada (Move j o), onde j seria o id do pacman e o a orientação original do pacman e utiliza-se um State, que corresponderia ao State atual que foi recebido na passTimePac.
-}
passTimePac :: Int  -> State -> State
passTimePac n st@(State (x:xs) (p:ps) l)
  |pacMode (getPacman (p:ps)) == Mega && mod n 4 == 0 = play (Move j o) ns
  |otherwise = play (Move j o) st
  where j = getPlayerID $ getPacman (p:ps)
        o = getPlayerOrientation $ getPacman (p:ps)
        player = (backToNormal $ timeMegaCD $ getPacman $ getPlayers st )
        ns = newState st player

{- | Função que diminui o timeMega do pacman em 1 consuante as jogadas

@
Mega -> (Pacman (PacState (id, (a,b), v, o, points, lifes) (timeMega - 1) openClosed pacmanMode)) 
@
*Inicialmente verifica-se qual o estado do pacman. Se este for Mega, retira-se 1 ao timeMega. 

@
_ -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))
@
*Caso o estado do pacman não seja Mega, não se faz nenhuma alteração.
-}
timeMegaCD :: Player -> Player 
timeMegaCD (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) = case pacmanMode of
                                                                                                    Mega -> (Pacman (PacState (id, (a,b), v, o, points, lifes) (timeMega - 1) openClosed pacmanMode)) 
                                                                                                    _ -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))

{- | Função que faz com que o pacman volte ao estado normal quando o timeMega atinge o 0

@
Mega -> if timeMega <= 0 then (Pacman (PacState (id, (a,b), v, o, points, lifes) 0 openClosed Normal))
@
*Inicialmente verifica-se qual o estado do pacman. Se este for Mega, dentro existe uma condição que verifica se o timeMega do pacman é igual ou inferior a zero. Se assim for, altera o estado do pacman para Normal.
*Caso o pacman esteja em estado Mega e o timeMega seja superior a 0, o estado mantém-se Mega.

@
_ -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))
@
*Caso o estado do Pacman não seja Mega, o estado mantém-se. 
-}
backToNormal :: Player -> Player 
backToNormal (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode)) = case pacmanMode of 
                                                                                                      Mega -> if timeMega <= 0 then (Pacman (PacState (id, (a,b), v, o, points, lifes) 0 openClosed Normal))
                                                                                                              else (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))
                                                                                                      _ -> (Pacman (PacState (id, (a,b), v, o, points, lifes) timeMega openClosed pacmanMode))



