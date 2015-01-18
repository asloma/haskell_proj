module Main where
import FileReader
import BoardOperator
import Solver

import Data.Matrix as M
import Data.List as L
import System.IO
 
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
                                          isEmptyString inputFile "Nie podano nazwy pliku wejsciowgo"
                                          readSolve inputFile
                                          main
                                'w' -> do                                        
                                          let inputFileDroppeed =  (L.dropWhile (== ' ') (L.dropWhile (/= ' ') commandDroppeed))      
                                          let outputFile = L.takeWhile (/= ' ') inputFileDroppeed --nazwa pliku wyj
                                          isEmptyString inputFile "Nie podano nazwy pliku wejsciowgo"
                                          isEmptyString outputFile "Nie podano nazwy pliku wyjsciowego"
                                          readSolveWrite inputFile outputFile
                                          main
                                _-> main
                                           
----------------------------------------
--wczytuje plik i wyswietla rozwiazanie w konsoli
readSolve :: [Char] -> IO ()
readSolve inputFile = 
                     do
                        --odczytanie zawartosci pliku
                        contentString <- readGameFile inputFile
                        contentList <- getContentList contentString
                        
                        --podzial zawartosci pliku na wiersze
                        let row1 = contentList !! 0
                        let row2 = contentList !! 1
                        let row3 = contentList !! 2
        
                        --sprawdzenie poprawności odczytanych wierszy
                        isIntegerList row1  
                        isIntegerList row2
                        isIntTopleList row3
                        
                        --rzutowanie wierszy na odpwiednie typy    
                        let intListLeft         = read row1 :: [Integer]
                        let intListTop          = read row2 :: [Integer]
                        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
                              
                        --okreslenie liczby kolumn i wierszy                    
                        let colsNumber = L.length intListTop
                        let rowsNumber = L.length intListLeft
                
                        --stworzenie planszy
                        let gameBoardRow = [toInteger(x-x) | x <- [1..colsNumber]]--replicate colsNumber 0        
                        let board = fromLists (L.replicate rowsNumber gameBoardRow)
                        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy
                        
                        --wczytane z pliku wiersze                          
                        --putStrLn(show intListLeft)
                        --putStrLn(show intListTop)
                        --putStrLn(show row3IntTupleList)
                        
                        putStrLn "------------------------------"
                        putStrLn "Plansza po wczytaniu:"        
                        printGameBoard  intListLeft intListTop (matrixToList gameBoard Main.debug) Main.debug
                        --putStrLn "Macierz solvera:"
                        --putStrLn(show gameBoard) 
                        
                        --obliczenie rozwiazania
                        let solution = solvePuzzles gameBoard  intListTop intListLeft
                        putStrLn "Rozwiazanie solvera:"
                        printGameBoard  intListLeft intListTop (matrixToList solution Main.debug) Main.debug
                                              
----------------------------------------
--wczytuje plik i zapisuje rozwiazanie do pliku 
readSolveWrite :: [Char] -> FilePath -> IO ()       
readSolveWrite inputFile outputFile = 
                                     do
                                        --odczytanie zawartosci pliku
                                        contentString <- readGameFile inputFile
                                        contentList <- getContentList contentString
          
                                        --podzial zawartosci pliku na wiersze
                                        let row1 = contentList !! 0
                                        let row2 = contentList !! 1
                                        let row3 = contentList !! 2
        
                                        --sprawdzenie poprawności odczytanych wierszy
                                        isIntegerList row1  
                                        isIntegerList row2
                                        isIntTopleList row3
                                        
                                        --rzutowanie wierszy na odpwiednie typy
                                        let intListLeft         = read row1 :: [Integer]
                                        let intListTop          = read row2 :: [Integer]
                                        let row3IntTupleList    = mapTuple (\x -> x+1) (read row3 :: [(Int, Int)]) --matrix numerowy od 1,1, a nie 0,0
        
                                        --okreslenie liczby kolumn i wierszy
                                        let colsNumber = L.length intListTop
                                        let rowsNumber = L.length intListLeft
                
                                        --stworzenie planszy
                                        let gameBoardRow = [toInteger(x-x) | x <- [1..colsNumber]]--replicate colsNumber 0        
                                        let board = fromLists (L.replicate rowsNumber gameBoardRow)
                                        let gameBoard = putHouses 1 1 board row3IntTupleList --domki umieszczone na planszy

                                        let solution = solvePuzzles gameBoard  intListTop intListLeft
                                        
                                        --zapis rozwiazania do pliku
                                        handle <- openFile outputFile WriteMode
                                        hPutStrLn handle "------------------------------"
                                        hPutStrLn handle "Rozwiazanie solvera:" 
                                        hPutStrLn handle "------------------------------"    
                                        hPutStrLn handle (retGameBoard  intListLeft intListTop (matrixToList solution Main.debug) Main.debug)
                                        hClose handle
                                        putStrLn ("Utworzono plik: " L.++ outputFile)