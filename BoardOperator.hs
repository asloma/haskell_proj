module BoardOperator where

import Data.Matrix as M
import Data.Vector as V
import Data.List as L

----------------------------------------
--drukuje wiersz z liczb¹ domkow w danym wierszu
printRow rowName elem = 
                        putStrLn ((if rowName < 10 then " " L.++ (show rowName) else show rowName) L.++  "  " L.++ L.concat (intersperse "  "  elem))

----------------------------------------
--drukuje listê domków z lewej strony tabeli gry i tabelê gry
printTable rowsNameList rowElemList = 
                                     Prelude.mapM_ (uncurry printRow) (L.zip rowsNameList rowElemList)

----------------------------------------
--formatuje listê domków na górze planszy
printHousesListTop :: [Int] -> String
printHousesListTop [] = []
printHousesListTop (x:xs) = if (x < 10)
                        then (show x) L.++ "  " L.++ printHousesListTop xs
                      else
                        (show x) L.++ " " L.++ printHousesListTop xs

----------------------------------------
--drukuje planszê gry z iloœci¹ domków (lista z lewej i na górze)
printGameBoard  rowsNameList colsNameList table =
                                             do
                                               putStrLn ("------------------------------")
                                               putStrLn ("    " L.++ printHousesListTop colsNameList)
                                               printTable rowsNameList table
                                               putStrLn ("Legenda: ")
                                               putStrLn ("h - domek ")
                                               putStrLn ("e - pole puste")
                                               putStrLn ("c - pole sprawdzone")
                                               putStrLn ("t - zbiornik")
                                               putStrLn ("------------------------------")

----------------------------------------
--uzupe³nia planszê o domki podane w liœcie (housesList)
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
--formatuje matrix do wyœwietlenia jako litery
replaceChar [] = []
replaceChar (x:xs) = 
                if x == '(' 
                  then '\"' : replaceChar xs 
                else if x == ')' 
                  then '\"' : replaceChar xs
                else if x == '\n' 
                  then ',' : replaceChar xs
                else if x == '0' 
                  then 'e' : replaceChar xs
                else if x == '1' 
                  then 'h' : replaceChar xs
                else if x == '3' 
                  then 't' : replaceChar xs  
                else if x == '9' 
                  then 'c' : replaceChar xs  
                else if x == ' ' 
                  then replaceChar xs 
                else x : replaceChar xs

----------------------------------------
--formatuje wiersz planszy do wyœwietlenia                
rowFormat :: String -> [String]              
rowFormat [] = []
rowFormat (x:xs) = [x] : rowFormat xs

----------------------------------------
--zamiana matrix na listê 2d do wyswietlenia
matrixToList matrix =
                       do
                         let str = L.init (L.tail ((L.init (replaceChar (show matrix)))))
                         let str2 = ("[ \"" L.++ str L.++"\" ]")
                         let str3 = read str2 :: [[Char]]
                         L.map rowFormat str3
