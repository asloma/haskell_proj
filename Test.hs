module Test where

import FileReader
import BoardOperator
import Solver
import Data.List as L
import Data.Matrix as M
import Data.Vector as V
 
----------------------------------------
--debug = 1 - wykonanie programu w trybie debugowania
--debug = 0 - wykonanie programu w zwyklym trybie
debug :: Integer
debug = 1

----------------------------------------
--wczytuje plik i wyswietla plansze z zaznaczonymi domkami
readFileShowBoardTest fPath = 
      do
        putStrLn "------------------------------------------------------------"
        putStrLn ("readFileShowBoardTest file: " L.++ fPath L.++ " START")
        contentString <- readGameFile fPath
        contentList <- getContentList contentString
          
        let row1 = contentList !! 0
        let row2 = contentList !! 1
        let row3 = contentList !! 2
        
        let intListLeft         = read row1 :: [Integer]
        let intListTop          = read row2 :: [Integer]
        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
        putStrLn ("Wczytany 1 wiersz lewa: " L.++ (show intListLeft))        
        putStrLn ("Wczytany 2 wiersz gora: " L.++ (show intListTop))
        putStrLn ("Wczytany 3 wiersz (+1,+1): " L.++ (show row3IntTupleList))

        let colsNumber = L.length intListTop
        let rowsNumber = L.length intListLeft
        
        putStrLn ("Kolumn: " L.++ (show colsNumber))
        putStrLn ("Wierszy: " L.++ (show rowsNumber))
        
        let gameBoardRow = [toInteger(x-x) | x <- [1..colsNumber]]--replicate colsNumber 0        
        let board = fromLists (L.replicate rowsNumber gameBoardRow)
        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy
        
        putStrLn "------------------------------"
        putStrLn "Plansza po wczytaniu:"        
        printGameBoard  intListLeft intListTop (matrixToList gameBoard debug) debug

        putStrLn ("readFileShowBoardTest file: " L.++ fPath L.++ " END")
        putStrLn "------------------------------------------------------------"

----------------------------------------
--test wczytywania wszystkich dostêpnych plansz
readFileTestAll = 
                 do
                   readFileShowBoardTest "test_files/p1.txt"
                   readFileShowBoardTest "test_files/p2.txt"
                   readFileShowBoardTest "test_files/p3.txt"
                   readFileShowBoardTest "test_files/p4.txt"
                   readFileShowBoardTest "test_files/p5.txt"
                   readFileShowBoardTest "test_files/p6.txt"
                   readFileShowBoardTest "test_files/p7.txt"

----------------------------------------
--wstawia zbiorniki z listy do macierzy                        
putTanks posX posY mat tanksList      | posX > V.length (getRow 1 mat) = putTanks 1 (posY+1) mat tanksList
                                      | posY > V.length (getCol 1 mat) = mat
                                      | otherwise =   
                                                    if L.elem (posX, posY) tanksList
                                                        then putTanks (posX+1) posY (setElem 3 (posX, posY) mat) tanksList
                                                    else putTanks (posX+1) posY mat tanksList    
----------------------------------------
--wczytuje i wyœwietla rozwiazanie z pliku                                                      
openSolutionAndShow intListLeft intListTop matrix fSolution = 
                                      do
                                        --putStrLn ("openSolutionAndShow START")
                                        contentString <- readGameFile fSolution
                                        let intSolList    = mapTuple (\x -> x+1) (read contentString :: [(Int, Int)])
                                        
                                        let gameBoard = putTanks 1 1 matrix intSolList 
             
                                        printGameBoardNoLegend  intListLeft intListTop (matrixToList gameBoard debug)

                                        --putStrLn ("openSolutionAndShow END")

----------------------------------------
--test solvera (œcie¿ka dane, scie¿ka rozwiazanie)                                        
solverTest fPath fSolPath = do
        putStrLn "------------------------------------------------------------"
        putStrLn ("solverTest file: " L.++ fPath L.++ " START")
        contentString <- readGameFile fPath
        contentList <- getContentList contentString
          
        let row1 = contentList !! 0
        let row2 = contentList !! 1
        let row3 = contentList !! 2
        
        let intListLeft         = read row1 :: [Integer]
        let intListTop          = read row2 :: [Integer]
        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
        let colsNumber = L.length intListTop
        let rowsNumber = L.length intListLeft
        
        let gameBoardRow = [toInteger(x-x) | x <- [1..colsNumber]]--replicate colsNumber 0        
        let board = fromLists (L.replicate rowsNumber gameBoardRow)
        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy
        
        let solution = solvePuzzles gameBoard intListTop intListLeft
        
        putStrLn "------------------------------"
        putStrLn "Rozwiazanie solvera:"
        printGameBoardNoLegend  intListLeft intListTop (matrixToList solution debug)
        putStrLn ("+ - pole sprawdzone")
        
        putStrLn "------------------------------"
        putStrLn "Rozwiazanie wczytane z pliku:"   
        openSolutionAndShow intListLeft intListTop gameBoard fSolPath
        putStrLn "------------------------------"
        putStrLn "Macierz solvera:"
        putStrLn(show solution) 
        putStrLn ("solverTest file: " L.++ fPath L.++ " END")
        putStrLn "------------------------------------------------------------"

----------------------------------------
--test solvera dla wszystkich dostêpnych plików          
solverTestAll = 
         do
          solverTest "test_files/p1.txt" "sol_files/sp1.txt"
          --solverTest "test_files/p2.txt" "sol_files/sp2.txt"
          --solverTest "test_files/p3.txt" "sol_files/sp3.txt"
          --solverTest "test_files/p4.txt" "sol_files/sp4.txt"
          --solverTest "test_files/p5.txt" "sol_files/sp5.txt"
          --solverTest "test_files/p6.txt" "sol_files/sp6.txt"
          --solverTest "test_files/p7.txt" "sol_files/sp7.txt"