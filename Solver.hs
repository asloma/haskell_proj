module Solver where
-- uzywasz tylko funkcji solvePuzzles mat xList yList 
-- ktora przyjmuje:
-- mat - plansza jako macierz Data.Matrix
-- xList - lista na osi ox macierzy (pozioma) z liczbami zbiornikow w danej kolumnie
-- yLisy - lista na osi oy macierzy (pionowa) z liczbami zbiornikow w danym wierszu - spisana od gory do dolu
-- symbole w macierzy: 0 - puste, 1 - dom, 3 - zbiornik, 9 - sprawdzone (nie bedzie zbiornika - X na obrazku)
-- ponizej przykladowe dane, odpowiadajace przypadkowi z przykladowego rozwiazania, ktore kiedys wyslalem Ci w mailu


import Data.Matrix
import Data.Vector
import Data.List

mat = fromLists [ [0,1,0,0,0,0],[0,0,0,0,1,0],[0,0,1,0,0,0],[0,1,0,1,0,0],[1,0,0,0,1,0],[0,0,1,0,0,0] ]
xList = [2,1,2,0,2,1]
yList = [1,2,0,3,0,2]

listFromIntegral list = Data.List.map fromIntegral list

sizesAreCorrect mat xList yList =       if Data.Vector.length (getRow 1 mat) == Prelude.length xList && Data.Vector.length (getCol 1 mat) == Prelude.length yList then 
                                                True 
                                        else    False

changeEmptyToChecked _ x =      if x == 0 then
                                9
                        else x

changeZeroToTank _ x =  if x == 0 then
                                3 
                        else x

isThisPlacePossibleHouse :: Int -> Int -> Matrix Integer -> Bool
isThisPlacePossibleHouse y x mat = 	(safeGetMatrix (y-1) x mat == 1) ||
					(safeGetMatrix (y+1) x mat == 1) || 
					(safeGetMatrix y (x-1) mat == 1) || 
					(safeGetMatrix y (x+1) mat == 1)

isLastPlaceAvalibleHouse :: Int -> Int -> Matrix Integer -> Bool
isLastPlaceAvalibleHouse y x mat =      let     left = x/=1
                                                right = x/= ncols mat
                                                top = y/=1
                                                down = y/= nrows mat
						avalible = left:right:top:down:[]
						avalibleNum = countFreqList True avalible
                                                checked = 	(safeGetMatrix (y-1) x mat == 9 || safeGetMatrix (y-1) x mat == 1 ):
								(safeGetMatrix (y+1) x mat == 9 || safeGetMatrix (y+1) x mat == 1 ):
								(safeGetMatrix y (x-1) mat == 9 || safeGetMatrix y (x-1) mat == 1 ):
								(safeGetMatrix y (x+1) mat == 9 || safeGetMatrix y (x+1) mat == 1 ):[]
						checkedNum = countFreqList True checked
					in	if( checkedNum == avalibleNum - 1) then True else False

processIsLastPlaceAvalible :: Int -> Int -> Matrix Integer -> Matrix Integer						
processIsLastPlaceAvalible y x mat =   		if (isLastPlaceAvalibleHouse y x mat) then
							if (safeGetMatrix y (x+1) mat == 0) then (setElem 3 (y-1,x) mat)
							else if (safeGetMatrix (y+1) x mat == 0) then (setElem 3 (y+1,x) mat)
							else if (safeGetMatrix y (x-1) mat == 0) then (setElem 3 (y,x-1) mat)
							else if (safeGetMatrix y (x+1) mat == 0) then (setElem 3 (y,x+1) mat)
							else mat
						else mat

isThisPlacePossibleTank :: Int -> Int -> Matrix Integer -> Bool
isThisPlacePossibleTank y x mat = 		not ( (safeGetMatrix (y-1) x mat == 3) ||  
                                                (safeGetMatrix (y-1) (x-1) mat == 3) ||  
                                                (safeGetMatrix (y-1) (x+1) mat == 3) ||  
                                                (safeGetMatrix (y+1) x mat == 3) || 
                                                (safeGetMatrix (y+1) (x-1) mat == 3) || 
                                                (safeGetMatrix (y+1) (x+1) mat == 3) || 
                                                (safeGetMatrix y (x-1) mat == 3) || 
                                                (safeGetMatrix y (x+1) mat == 3) )

safeGetMatrix :: Int -> Int -> Matrix Integer -> Integer
safeGetMatrix y x mat	| x<1 = 8
			| y<1 = 8
			| x>ncols mat = 8
			| y>nrows mat = 8
			| otherwise = mat Data.Matrix.!	(y,x)

countFreq elem vec = Data.Vector.length (Data.Vector.filter (\x -> x==elem) vec)

countFreqList elem list = Data.List.length (Data.List.filter (\x -> x==elem) list)

processRows mat list = processElemFunInt 1 mat (listFromIntegral list) getRow mapRow

processCols mat list = processElemFunInt 1 mat (listFromIntegral list) getCol mapCol

--po wierszach, pionowa lista z numerami
processElemFunInt pos mat list getFun mapFun    | pos >= Prelude.length list    = mat
                                                        | otherwise =
                                                                let numTanks = list !! (pos-1)
                                                                in 
                                                                        if countFreq 3 (getFun pos mat) == numTanks
                                                                        then processElemFunInt (pos+1) (mapFun changeEmptyToChecked (pos) mat) list getFun mapFun
                                                                        else    if ( countFreq 0 (getFun pos mat) + countFreq 3 (getFun pos mat) ) <= numTanks
                                                                                then processElemFunInt (pos+1) (mapFun changeZeroToTank (pos) mat) list getFun mapFun
                                                                                else processElemFunInt (pos+1) mat list getFun mapFun

checkNeighbours mat = iterateThrMatrix 1 1 mat

checkHousesLastPlaces mat = iterateThrMatrix2 1 1 mat

iterateThrMatrix posY posX mat  | posX > Data.Vector.length (getRow 1 mat) = iterateThrMatrix (posY+1) 1 mat
                                | posY > Data.Vector.length (getCol 1 mat) = mat
                                | otherwise =   let elem = mat Data.Matrix.! (posY,posX)
                                                in
                                                        if elem == 0
                                                        then    if (isThisPlacePossibleHouse posY posX mat && isThisPlacePossibleTank posY posX mat)
                                                                then iterateThrMatrix posY (posX+1) mat 
                                                                else iterateThrMatrix posY (posX+1) (setElem 9 (posY, posX) mat)
                                                        else iterateThrMatrix posY (posX+1) mat 

iterateThrMatrix2 posY posX mat | posX > Data.Vector.length (getRow 1 mat) = iterateThrMatrix2 (posY+1) 1 mat
                                | posY > Data.Vector.length (getCol 1 mat) = mat
                                | otherwise =   let elem = mat Data.Matrix.! (posY, posX)
                                                in
                                                        if elem == 1
                                                        then iterateThrMatrix2 posY (posX+1) (processIsLastPlaceAvalible posY posX mat)   
                                                        else iterateThrMatrix2 posY (posX+1) mat 


matElem x mat = Prelude.elem x (Data.Matrix.toList mat)

solvePuzzles mat xList yList    | not (matElem 0 mat) = mat
                                | otherwise = 
                                        --solvePuzzles (checkSharedTanks(checkNeighbours (checkSharedTanks(processCols (checkSharedTanks (processRows mat yList)) xList)))) xList yList -- po kazdym wolaniu funkcji jednej z tych 3 funkcji dac inne iterateThrMatrix ktore sprawdza domki, i jesli maja w sasiedztwie zbiornik
                                        solvePuzzles (checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) ) xList yList  
                                        


