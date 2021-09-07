module Tests where 

import Tarefa1
import Tarefa2
import Tarefa3
import Types 


testsT1 :: [(Int,Int,Int)]
testsT1 = [(21,15,2),(22,15,3),(21,16,4),(22,16,5)]

-- | Função que testa largura e altura do labirinto
--
testMaze :: (Int,Int,Int) -> Bool
testMaze (l,a,s) = let maze = generateCompleteMaze l a s 
                   in (length maze == a) && (length (head maze) == l) 

-- | Função que testa se o primeiro e o último corredor são só de paredes
--
testCorridorWalls :: (Int,Int,Int) -> Bool
testCorridorWalls (l,a,s) = let maze = generateCompleteMaze l a s
                            in (head maze == corridorWall l) && (last maze == corridorWall l)
-- | Função que testa se o labirinto é limitado lateralmente por paredes, excluindo os corredores que possuem túnel
--
wallsMaze :: (Int,Int,Int) -> Bool
wallsMaze (l,a,s) = let x = supMaze l a s 
                        y = middleSupMaze l a s 
                        z = middleInfMaze l a s 
                        p = infMaze l a s 
                    in (wallsLimit x) && (wallsLimit y) && (wallsLimit z) && (wallsLimit p)
-- | Função auxiliar que testa se o labirinto é limitado lateralmente por paredes
--
wallsLimit :: Maze -> Bool
wallsLimit [] = True 
wallsLimit (x:xs) = (x == ([Wall] ++ init (drop 1 x) ++ [Wall])) && wallsLimit xs 

-- | Função que testa se há túneis nos lugares certos
--
tunnel :: Maze -> Bool 
tunnel (x:xs) = if odd (length (x:xs)) then (idpieceCoord (x:xs) (middleMaze (x:xs),0) == Empty) && (idpieceCoord (x:xs) (middleMaze (x:xs),(length x)-1) == Empty)
                else (idpieceCoord (x:xs) (middleMaze (x:xs),0) == Empty) && (idpieceCoord (x:xs) (middleMaze (x:xs),(length x)-1) == Empty) && (idpieceCoord (x:xs) ((middleMaze (x:xs))-1,0) == Empty) && (idpieceCoord (x:xs) ((middleMaze (x:xs))-1,(length x)-1) == Empty)

-- | Função que testa se as coordenadas depois da jogada estão certas. Sendo (a,b) a coordenada antes da jogada e i 1 ou -1 dependendo da orientação que pretende ir
--
testCoords :: Play -> Maze -> [Player] -> Coords -> Int -> Bool
testCoords (Move j o) (x:xs) (p:ps) (a,b) i = if o == getPlayerOrientation (findPlayer j (p:ps)) then 
                                                       case idpieceCoord (x:xs) (a,b) of 
                                                            Food Big -> if coordsafter == (a,b+i) then True else if coordsafter == (a+i,b) then True else False 
                                                            Food Little -> if coordsafter == (a,b+i) then True else if coordsafter == (a+i,b) then True else False
                                                            Empty -> case isitTunnel (Move j o) (x:xs) (findPlayer j (p:ps)) of
                                                                        True -> if coordsafter == (a,0) then True else if coordsafter == (a,(length x)-1) then True else False 
                                                                        False -> if coordsafter == (a,b+i) then True else False 
                                                            Wall -> if coordsafter == (a,b) then True else False
                                              else case coordsafter of 
                                                      (a,b) -> True 
   where coordsafter = getCoords (movePlay (Move j o) (x:xs) (p:ps))

-- | Função que verifica se o espaço vazio é túnel
--
isitTunnel :: Play -> Maze -> Player -> Bool
isitTunnel (Move j o) (x:xs) (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) = if o == R then 
                                                                                                                            if (a,b) == (a,(length x)-1)
                                                                                                                            then True
                                                                                                                            else False
                                                                                                                    else 
                                                                                                                        if (a,b) == (a,0) 
                                                                                                                        then True
                                                                                                                        else False


-- | Função que testa se o pacman ficou Mega quando deveria ficar
--
testMega :: Play -> Maze -> [Player] -> Bool
testMega (Move j o) (x:xs) (p:ps) = if idpieceCoord (x:xs) coord == Food Big then pacMode (movePlay (Move j o) (x:xs) (p:ps)) == Mega else False 
  where coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))

-- | Função que testa se os fantasmas ficam mortos e com velocidade reduzida quando o pacman está mega
--
testMegaGhosts :: Play -> State -> Bool
testMegaGhosts (Move j o) (State (x:xs) (p:ps) l) = allGhostsDead (getGhosts (p:ps)) 

-- | Função que devolve o ghostMode do fantasma
--
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState (id, (a,b), v, o, points, lifes) ghostMode)) = ghostMode 

-- | Função que verifica se todos os fantasmas estão mortos
--
allGhostsDead :: [Player] -> Bool
allGhostsDead [] = True
allGhostsDead (g:gs) 
  |getGhostMode g == Dead = allGhostsDead gs  
  |otherwise = False 

-- | Função que testa se os pontos estão certos após a jogada
--
testPoints :: Play -> Maze -> [Player] -> Bool
testPoints (Move j o) (x:xs) (p:ps) =  case isthereGhost' (p:ps) coord of
                                          True -> case pacMode (findPlayer j (p:ps)) of
                                                    Mega -> if pacpointsafter == pacpoints + 10 then True else False 
                                                    _ -> if pacpointsafter == pacpoints then True else False 
                                          False -> case idpieceCoord (x:xs) coord of 
                                                       Food Big -> if pacpointsafter == pacpoints + 5 then True else False 
                                                       Food Little -> if pacpointsafter == pacpoints + 1 then True else False 
                                                       Empty -> if pacpointsafter == pacpoints then True else False 
                                                       Wall -> if pacpointsafter == pacpoints then True else False 

   where coord = getCoords (movePlay (Move j o) (x:xs) (p:ps))
         pacpoints = getPoints (findPlayer j (p:ps))
         pacpointsafter = getPoints (movePlay (Move j o) (x:xs) (p:ps))

-- | Função que devolve os pontos do jogador
--
getPoints :: Player -> Int
getPoints (Pacman (PacState (id, (a,b), v, or, points, lifes) timeMega openClosed pacmanMode)) = points 

