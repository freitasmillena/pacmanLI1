module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa4
import Tarefa2

data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer --0<delta<250 ms 4 jogadas num segundo, pacman andará em 1 seg aproximadamente 4 casas
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k man@(Manager st pid step before delta delay ) = (Manager newst pid step before delta delay )
    where players = getPlayers st
          player = findPlayer pid players
          newplayer = rotatePlayer k player 
          newst = updateState st newplayer 
      

rotatePlayer :: Key -> Player -> Player
rotatePlayer k (Pacman (PacState (id, (a,b), v, ori, points, lifes) timeMega openClosed pacmanMode)) = case k of 
                                                                                                          KeyUpArrow -> (Pacman (PacState (id, (a,b), v, U, points, lifes) timeMega openClosed pacmanMode))
                                                                                                          KeyDownArrow -> (Pacman (PacState (id, (a,b), v, D, points, lifes) timeMega openClosed pacmanMode))
                                                                                                          KeyLeftArrow -> (Pacman (PacState (id, (a,b), v, L, points, lifes) timeMega openClosed pacmanMode))
                                                                                                          KeyRightArrow -> (Pacman (PacState (id, (a,b), v, R, points, lifes) timeMega openClosed pacmanMode))
                                                                                                          _-> (Pacman (PacState (id, (a,b), v, Null, points, lifes) timeMega openClosed pacmanMode))
rotatePlayer k (Ghost (GhoState (id, (a,b), v, ori, points, lifes) ghostMode)) = case k of 
                                                                                  KeyUpArrow -> (Ghost (GhoState (id, (a,b), v, U, points, lifes) ghostMode))
                                                                                  KeyDownArrow -> (Ghost (GhoState (id, (a,b), v, D, points, lifes) ghostMode))
                                                                                  KeyLeftArrow -> (Ghost (GhoState (id, (a,b), v, L, points, lifes) ghostMode))
                                                                                  KeyRightArrow -> (Ghost (GhoState (id, (a,b), v, R, points, lifes) ghostMode))
                                                                                  _-> (Ghost (GhoState (id, (a,b), v, Null, points, lifes) ghostMode))

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      --clear 
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime -- converte pra ms multiplicando por 1000 e depois arredonda as unidades pra dar um valor inteiro em ms

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager st pid step before delta delay ) = (Manager st pid step before newdelta delay )  -- delta = now - before 
  where newdelta = now - before 

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager st pid step before delta delay ) = (Manager st pid step now 0 delay )  --delta = 0 before = now

nextFrame :: Integer -> Manager -> Manager 
nextFrame now (Manager st pid step before delta delay ) = resetTimer now (Manager newst pid step now delta delay)
  where newst = passTime step st 


loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorWhite ColorDefault  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

