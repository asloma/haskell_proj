module Main where
import FileReader
import BoardOperator
import Solver

import Data.Matrix as M
import Data.Vector
import Data.List as L
import Test
           
main::IO()
main = 
      do
        contentString <- readGameFile "test_files/p1.txt"
        contentList <- getContentList contentString
          
        let row1 = contentList !! 0
        let row2 = contentList !! 1
        let row3 = contentList !! 2
        
        let intListLeft         = read row1 :: [Int]
        let intListTop          = read row2 :: [Int]
        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
        putStrLn ("Wczytany 1 wiersz lewa: " L.++ (show intListLeft))        
        putStrLn ("Wczytany 2 wiersz gora: " L.++ (show intListTop))
        putStrLn ("Wczytany 3 wiersz (+1,+1): " L.++ (show row3IntTupleList))

        let colsNumber = L.length intListTop
        let rowsNumber = L.length intListLeft
        
        putStrLn ("Kolumn: " L.++ (show colsNumber))
        putStrLn ("Wierszy: " L.++ (show rowsNumber))
        
        let gameBoardRow = [x-x | x <- [1..colsNumber]]--replicate colsNumber 0        
        let board = fromLists (L.replicate rowsNumber gameBoardRow)
        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy
        
        putStrLn "------------------------------"
        putStrLn "Plansza po wczytaniu:"        
        printGameBoard  intListLeft intListTop (matrixToList gameBoard)

        let solution = solvePuzzles gameBoard  intListTop intListLeft
        putStrLn "Rozwiazanie:"
        printGameBoard  intListLeft intListTop (matrixToList solution)
        putStrLn(show solution)
        putStrLn "Koniec"