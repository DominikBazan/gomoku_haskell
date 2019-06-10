-- Dominik Bazan
-- Gomoku random
-- type 'start' to play

module Gomoku where

import System.Random
import Data.Char

data Board x = Board [[x]] deriving Show

data Turn = Me | Pc deriving (Show,Eq)

data Player = Empty | Cross | Circle deriving (Show,Eq)

initBoard :: Int -> Board Player
initBoard size = Board (replicate size (replicate size Empty))

chPlayer :: Player -> Player
chPlayer play = if play == Cross then Circle else do Cross

--Display:

index :: [[Player]] -> Int -> IO ()
index [] size = return ()
index (width:tail) size
                       | size <  10 = putStr(" " ++ show size ++ " ") >> 
                         drawSign width size  >> index tail (size+1)
                       | size >= 10 = putStr( show size ++ " ") >> 
                         drawSign width size  >> index tail (size+1)

drawSign :: [Player] -> Int -> IO ()
drawSign [] size = putStr("") >> print size   
drawSign (width:tail) size | width == Cross = putStr " X " >> drawSign tail size
                           | width == Circle = putStr " O " >> drawSign tail size
                           | width == Empty = putStr " - " >> drawSign tail size

upperIndeks :: [[Player]] -> Int -> IO ()
upperIndeks [] size = putStrLn ""
upperIndeks (width:tail) size
                      | size == 1 = putStr("    " ++ show size) >> upperIndeks tail (size+1)
                      | size <= 10 = putStr("  " ++ show size ) >> upperIndeks tail (size+1)
                      | size > 10 = putStr(" " ++ show size ) >> upperIndeks tail (size+1)

drawBoard :: Board Player -> IO ()
drawBoard (Board (x:tail)) =  upperIndeks (x:tail) 1
                            >> index (x:tail) 1
                            >> upperIndeks (x:tail) 1

--Inserting:

insert :: Board Player -> Player -> Int -> Int -> Board Player
insert (Board a) sign row col = Board ( take (row-1) a  
    ++ [take (col-1) (a !! (row-1)) ++ [sign] ++ drop col (a !! (row-1))  ]         
    ++ drop row a)


--Final conditions of the game:

--one row
chRow :: [Player] -> Player -> Int -> Bool
chRow [] p size = False
chRow (h:t) p size = do
    if size >= 5 then do
        if (h==p) && ((t !! 0) == p) && ((t !! 1) == p) && ((t !! 2) == p) && ((t !! 3) == p)
            then True
            else chRow (t) p (size-1)
    else False

--all rows
chAllRows :: Board Player -> Player -> Int -> Int -> Bool
chAllRows (Board x) (p) size counter = do
    if size /= (counter) 
      then do
        if (chRow (x !! counter) (p) size)
            then True
        else chAllRows (Board x) (p) size (counter+1)
      else False

--one column
chCol :: [[Player]] -> Player -> Int -> Int -> Bool
chCol [] p size kol = False
chCol (h:t) p size kol = do
    if size >= 5 then do
        if ((h !! (kol))==p) && (((t !! 0) !! (kol))== p) && (((t !! 1) !! (kol)) == p) && (((t !! 2) !! (kol)) == p) && (((t !! 3) !! (kol)) == p)
            then True
            else chCol (t) p (size-1) kol
    else False

--all columns
chAllCol :: Board Player -> Player -> Int -> Int -> Bool            
chAllCol (Board x) p size counter = do
    if size /= (counter) 
      then do
        if (chCol (x) (p) size counter)
            then True
        else chAllCol (Board x) (p) size (counter+1)
      else False

-- diagonal LR
chDiagonalLR :: [[Player]] -> Player -> Int -> Int -> Bool
chDiagonalLR [] p size kol = False
chDiagonalLR (h:t) p size kol = do
    if size >= 5 then do
        if ((h !! (kol))==p) && (((t !! 0) !! (kol+1))== p) 
            && (((t !! 1) !! (kol+2)) == p) && (((t !! 2) !! (kol+3)) == p) 
            && (((t !! 3) !! (kol+4)) == p)
            then True
            else chDiagonalLR (t) p (size-1) kol
    else False

-- all diagonal LR
chAllDiagonalLR :: Board Player -> Player -> Int -> Int -> Bool            
chAllDiagonalLR (Board x) p size counter = do
    if size > (counter+4)
      then do
        if (chDiagonalLR (x) (p) size counter)
            then True
        else chAllDiagonalLR (Board x) (p) size (counter+1)
      else False

-- diagonal RL
chDiagonalRL :: [[Player]] -> Player -> Int -> Int -> Bool
chDiagonalRL [] p size kol = False
chDiagonalRL (h:t) p size kol = do
    if size >= 5 then do
        if ((h !! (kol+4))==p) && (((t !! 0) !! (kol+3))== p) 
            && (((t !! 1) !! (kol+2)) == p) && (((t !! 2) !! (kol+1)) == p) 
            && (((t !! 3) !! (kol)) == p)
            then True
            else chDiagonalRL (t) p (size-1) kol
    else False

-- all diagonal RL
chAllDiagonalRL :: Board Player -> Player -> Int -> Int -> Bool            
chAllDiagonalRL (Board x) p size counter = do
    if size > (counter+5)
      then do
        if (chDiagonalRL (x) (p) size (counter))
            then True
        else chAllDiagonalRL (Board x) (p) size (counter+1)
      else False

-- check if there is a win
chIfWin :: Board Player -> Player -> Int -> Bool
chIfWin (Board x) (p) size = do
    if (chAllRows (Board x) (p) size 0)
      then True
      else 
        if (chAllCol (Board x) (p) size 0)
          then True
          else 
            if (chAllDiagonalLR (Board x) (p) size 0)
              then True
              else 
                if (chAllDiagonalRL (Board x) (p) size 0)
                  then True
                  else False

-- if suitable string
isStrInt :: String -> Bool
isStrInt "" = False
isStrInt "." = False
isStrInt t =
  case dropWhile isDigit t of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

-- number input
getInt :: IO Int
getInt = do
    op <- getLine
    if isStrInt op
            then 
                let signInt = read op :: Int in 
                return signInt
            else do
                putStr "Type suitable number..."
                getInt  
      
-- check if occupied
checkIfTaken :: Board Player -> Int -> Int -> Bool
checkIfTaken (Board x) wier kol = do
    if ((x !! wier) !! kol) == Empty then False else True


play :: Board Player -> Turn -> Player -> IO ()
-- PC move
play (Board x) (Pc) (p) = do
    putStrLn "\n\n"
    drawBoard (Board x)     
    if (chIfWin (Board x) (chPlayer p) 19) 
      then 
        putStrLn "Player wins !"
      else do
        row <- randomRIO(1,19)
        col <- randomRIO(1,19)
        if (checkIfTaken (Board x) (row-1) (col-1)) 
          then 
            play (Board x) (Pc) (p)
          else
            play (insert (Board x) (p) row col) (Me) (chPlayer p)
-- Player move
play (Board x) (Me) (p) = do 
     putStrLn "\n\n"
     drawBoard (Board x)
     if chIfWin (Board x) (chPlayer p) 19 
       then putStrLn "PC Wins :o"
       else do
         putStr "Row... "
         row <- getInt
         putStr "Column... "
         col <- getInt
         if (checkIfTaken (Board x) (row-1) (col-1))
           then do
             putStrLn "This field isn't empty!"
             play (Board x) (Me) (p)
           else
             play (insert (Board x) (p) row col) (Pc) (chPlayer p)


-- menu
chooseGame :: String -> IO ()
chooseGame op
  | op == "1" = play (initBoard 19) (Me) (Circle)
  | op == "2" = play (initBoard 19) (Pc) (Circle)
  | op == "0" = putStrLn "Exit."
  | otherwise = start


start :: IO ()
start = do
    putStrLn "####################################"
    putStrLn "##########   Gra Gomoku   ##########"
    putStrLn "####################################"
    putStrLn ""
    putStrLn "           Wybierz strone:"
    putStrLn "               1 - O"
    putStrLn "               2 - X"
    putStrLn ""
    putStrLn "              0 - Exit"
    putStrLn ""
    putStr   "              Options... "
    op <- getLine
    chooseGame op
