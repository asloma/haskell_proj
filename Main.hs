module Main where
import FileReader
import BoardOperator
import qualified Data.Text as T

main::IO()
main = 
      do
        file <- readGameFile "p1.txt"
        let row1 = file !! 0
        let row2 = file !! 1
        let row3 = file !! 2
        
        let intListLeft         = getIntListFromString row1
        let intListTop          = getIntListFromString row2
        let row3IntTupleList    = getTupleListFromString row3
        
        putStrLn ("1 wiersz lewa: " ++ (show intListLeft))        
        putStrLn ("2 wiersz góra: " ++ (show intListTop))
        putStrLn ("3 wiersz: " ++ (show row3IntTupleList))

        let colsNumber = length intListTop
        let rowsNumber = length intListLeft
        
        putStrLn ("Kolumn: " ++ (show colsNumber))
        putStrLn ("Wierszy: " ++ (show rowsNumber))
        
        let gameBoardRow = replicate colsNumber "ee" 
        putStrLn (show gameBoardRow)
        
        let gameBoard = replicate rowsNumber gameBoardRow
        putStrLn (show gameBoard)
        
        printBoard intListLeft intListTop gameBoard
        
        let gameBoardT = gameBoard
        let gameBoard = changeElem gameBoardT 8 4 "hh"
        printBoard intListLeft intListTop gameBoard
        
        let gameBoardT = gameBoard
        let gameBoard = changeElem gameBoardT 0 0 "hh"
        printBoard intListLeft intListTop gameBoard
        
        --let gameBoardT = gameBoard
        --let gameBoard = mark row3IntTupleList gameBoardT
        --printBoard intListLeft intListTop gameBoard
        putStrLn "Koniec"