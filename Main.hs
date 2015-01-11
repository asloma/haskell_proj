module Main where
import FileReader
import BoardOperator
import Solver

import Data.Matrix as M
import Data.List as L
import System.IO
import Test()

----------------------------------------
--debug = 1 - wykonanie programu w trybie debugowania
--debug = 0 - wykonanie programu w zwyklym trybie
debug :: Integer
debug = 0

----------------------------------------
--obsluguje menu programu
main :: IO ()
main = do
           
           putStrLn "\nWybierz polecenie:"
           putStrLn ("------------------------------------------------------------------------------------------")
           putStrLn " r NazwaPlikuWej               - wczytaj plik NazwaPlikuWej, wyswietl rozwiazanie w konsoli"
           putStrLn " w NazwaPlikuWej NazwaPlikuWyj - wczytaj plik NazwaPlikuWej, zapisz rozwiazanie do pliku NazwaPlikuWyj"
           putStrLn " e                             - koniec"
           putStrLn ("------------------------------------------------------------------------------------------")
           
           cmd <- getLine
           let cmd2 = L.dropWhile (== ' ') cmd --usuniecie spacji na poczatku     
           let command = L.takeWhile (/= ' ') cmd2 --polecenie           
           let commandDroppeed = L.dropWhile (== ' ') (L.dropWhile (/= ' ') (L.dropWhile (== ' ') cmd2))          
           let inputFile = L.takeWhile (/= ' ') commandDroppeed --nazwa pliku wej        
                                                     
           case command !! 0 of
                                'e' -> return ()
                                'r' -> do
                                          readSolve inputFile
                                          main
                                'w' -> do                                        
                                          let inputFileDroppeed =  (L.dropWhile (== ' ') (L.dropWhile (/= ' ') commandDroppeed))      
                                          let outputFile = L.takeWhile (/= ' ') inputFileDroppeed --nazwa pliku wyj
                                          readSolveWrite inputFile outputFile
                                          main
                                _-> main
                                           
----------------------------------------
--wczytuje plik i wyswietla rozwiazanie w konsoli
readSolve :: [Char] -> IO ()
readSolve inputFile = 
                     do
                        contentString <- readGameFile inputFile
                        contentList <- getContentList contentString
          
                        let row1 = contentList !! 0
                        let row2 = contentList !! 1
                        let row3 = contentList !! 2
        
                        let intListLeft         = read row1 :: [Int]
                        let intListTop          = read row2 :: [Int]
                        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
                        let colsNumber = L.length intListTop
                        let rowsNumber = L.length intListLeft
                
                        let gameBoardRow = [x-x | x <- [1..colsNumber]]--replicate colsNumber 0        
                        let board = fromLists (L.replicate rowsNumber gameBoardRow)
                        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy
        
                        putStrLn "------------------------------"
                        putStrLn "Plansza po wczytaniu:"        
                        printGameBoard  intListLeft intListTop (matrixToList gameBoard Main.debug) Main.debug

                        let solution = solvePuzzles gameBoard  intListTop intListLeft
                        putStrLn "Rozwiazanie solvera:"
                        printGameBoard  intListLeft intListTop (matrixToList solution Main.debug) Main.debug
                        
----------------------------------------
--wczytuje plik i zapisuje rozwiazanie do pliku 
readSolveWrite :: [Char] -> FilePath -> IO ()       
readSolveWrite inputFile outputFile = 
                                     do
                                        contentString <- readGameFile inputFile
                                        contentList <- getContentList contentString
          
                                        let row1 = contentList !! 0
                                        let row2 = contentList !! 1
                                        let row3 = contentList !! 2
        
                                        let intListLeft         = read row1 :: [Int]
                                        let intListTop          = read row2 :: [Int]
                                        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
                                        let colsNumber = L.length intListTop
                                        let rowsNumber = L.length intListLeft
                
                                        let gameBoardRow = [x-x | x <- [1..colsNumber]]--replicate colsNumber 0        
                                        let board = fromLists (L.replicate rowsNumber gameBoardRow)
                                        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy

                                        let solution = solvePuzzles gameBoard  intListTop intListLeft
                                        
                                        handle <- openFile outputFile WriteMode
                                        hPutStrLn handle "------------------------------"
                                        hPutStrLn handle "Rozwiazanie solvera:" 
                                        hPutStrLn handle "------------------------------"    
                                        hPutStrLn handle (retGameBoard  intListLeft intListTop (matrixToList solution Main.debug) Main.debug)
                                        hClose handle
                                        putStrLn ("Utworzono plik: " L.++ outputFile)