module BoardOperator where

import Data.Matrix as M
import Data.Vector as V
import Data.List as L
import System.IO

----------------------------------------
--zwraca wiersz z liczb� domkow w danym wierszu
retRow rowName elem = 
                        (if rowName < 10 then " " L.++ (show rowName) else show rowName) L.++  "  " L.++ L.concat (intersperse "  "  elem)
               
               ----------------------------------------
--zwraca list� domk�w z lewej strony tabeli gry i tabel� gry
retTable rowsNameList rowElemList = 
                                     Prelude.map (uncurry retRow) (L.zip rowsNameList rowElemList)

----------------------------------------
--zwraca plansz� gry z ilo�ci� domk�w (lista z lewej i na g�rze)
retGameBoardNoLegend  rowsNameList colsNameList table =                                           
                                                ("    " L.++ printHousesListTop colsNameList) L.++ "\n" L.++
                                                ((unlines (retTable rowsNameList table))) 
                                                
----------------------------------------
--zwraca plansz� gry z ilo�ci� domk�w (lista z lewej i na g�rze) + legenda
retGameBoard  rowsNameList colsNameList table debug =
                                               if debug == 1 then
                                                                retGameBoardNoLegend rowsNameList colsNameList table
                                                                L.++ ("Legenda: ") L.++ "\n"
                                                                L.++ ("h - domek ") L.++ "\n"
                                                                L.++ ("t - zbiornik") L.++ "\n"
                                                                L.++  ("* - pole puste") L.++ "\n"
                                                                L.++ ("+ - pole sprawdzone") L.++ "\n"
                                               else
                                                                retGameBoardNoLegend rowsNameList colsNameList table
                                                                L.++ ("Legenda: ") L.++ "\n"
                                                                L.++ ("h - domek ") L.++ "\n"
                                                                L.++ ("t - zbiornik") L.++ "\n"
                                                                L.++  ("* - pole puste") L.++ "\n"                                                                                                                                   
----------------------------------------
--drukuje wiersz z liczb� domkow w danym wierszu
printRow rowName elem = 
                        putStrLn ((if rowName < 10 then " " L.++ (show rowName) else show rowName) L.++  "  " L.++ L.concat (intersperse "  "  elem))

----------------------------------------
--drukuje list� domk�w z lewej strony tabeli gry i tabel� gry
printTable rowsNameList rowElemList = 
                                     Prelude.mapM_ (uncurry printRow) (L.zip rowsNameList rowElemList)

----------------------------------------
--formatuje list� domk�w na g�rze planszy
printHousesListTop :: [Int] -> String
printHousesListTop [] = []
printHousesListTop (x:xs) = if (x < 10)
                        then (show x) L.++ "  " L.++ printHousesListTop xs
                      else
                        (show x) L.++ " " L.++ printHousesListTop xs

----------------------------------------
--drukuje plansz� gry z ilo�ci� domk�w (lista z lewej i na g�rze)
printGameBoardNoLegend  rowsNameList colsNameList table =
                                             do
                                               putStrLn ("    " L.++ printHousesListTop colsNameList)
                                               printTable rowsNameList table
                                               
                                               
----------------------------------------
--drukuje plansz� gry z ilo�ci� domk�w (lista z lewej i na g�rze) + legenda
printGameBoard  rowsNameList colsNameList table debug =
                                             do
                                               putStrLn ("------------------------------")
                                               printGameBoardNoLegend rowsNameList colsNameList table
                                               putStrLn ("Legenda: ")
                                               if debug == 1 then do
                                                                putStrLn ("h - domek ")
                                                                putStrLn ("t - zbiornik")
                                                                putStrLn ("* - pole puste")                                                                
                                                                putStrLn ("+ - pole sprawdzone")
                                                else do
                                                                putStrLn ("h - domek ")
                                                                putStrLn ("t - zbiornik")
                                                                putStrLn ("* - pole puste") 
                                               putStrLn ("------------------------------")

----------------------------------------
--uzupe�nia plansz� o domki podane w li�cie (housesList)
putHouses posX posY mat housesList    | posX > V.length (getRow 1 mat) = putHouses 1 (posY+1) mat housesList
                                      | posY > V.length (getCol 1 mat) = mat
                                      | otherwise =   
                                                    if L.elem (posX, posY) housesList
                                                        then putHouses (posX+1) posY (setElem 1 (posX, posY) mat) housesList
                                                    else putHouses (posX+1) posY mat housesList

----------------------------------------
--map dla krotek
mapTuple f [] = []
mapTuple f (x:xs) = (f (fst x), f (snd x)) : (mapTuple f xs)

----------------------------------------
--formatuje matrix do wy�wietlenia jako litery
replaceChar [] debug = []
replaceChar (x:xs) debug = 
                if x == '(' 
                  then '\"' : replaceChar xs debug
                else if x == ')' 
                  then '\"' : replaceChar xs debug
                else if x == '\n' 
                  then ',' : replaceChar xs debug
                else if x == '0' 
                  then '*' : replaceChar xs debug
                else if x == '1' 
                  then 'h' : replaceChar xs debug
                else if x == '3' 
                  then 't' : replaceChar xs debug 
                else if x == '9' 
                  then if debug == 1 then '+' : replaceChar xs debug
                       else '*' : replaceChar xs debug                       
                else if x == ' ' 
                  then replaceChar xs debug
                else x : replaceChar xs debug

----------------------------------------
--formatuje wiersz planszy do wy�wietlenia                
rowFormat :: String -> [String]              
rowFormat [] = []
rowFormat (x:xs) = [x] : rowFormat xs

----------------------------------------
--zamiana matrix na list� 2d do wyswietlenia
matrixToList matrix debug =
                       do
                         let str = L.init (L.tail ((L.init (replaceChar (show matrix) debug))))
                         let str2 = ("[ \"" L.++ str L.++"\" ]")
                         let str3 = read str2 :: [[Char]]
                         L.map rowFormat str3
