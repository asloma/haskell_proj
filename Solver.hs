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
import System.Random

----------------------------------------
--testowe dane
mat = fromLists [ [0,1,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,1,0,1,0],[1,0,0,0,1,0],[0,0,1,0,0,1] ]
xList = [1,1,2,1,1,1]
yList = [1,0,2,1,2,1]



listFromIntegral list = Data.List.map fromIntegral list

----------------------------------------
--funkcja uzywana do mapowania - zmienia puste miejsca na sprawdzone 
changeEmptyToChecked _ x =	if x == 0 
				then 9
                        	else x

----------------------------------------
--funkcja uzywana do mapowania - zmienia puste miejsca na zbiorniki
changeEmptyToTank _ x =	if x == 0 
			then 3 
                        else x

----------------------------------------
--sprawdza czy puste miejsce o danych wspolrzednych w danej macierzy jest potencjalnym miejscem na zbiornik dzieki sasiedztwu domku
isThisPlacePossibleHouse :: Int -> Int -> Matrix Integer -> Bool
isThisPlacePossibleHouse y x mat = 	(safeGetMatrix (y-1) x mat == 1) ||
					(safeGetMatrix (y+1) x mat == 1) || 
					(safeGetMatrix y (x-1) mat == 1) || 
					(safeGetMatrix y (x+1) mat == 1)

----------------------------------------
--sprawdza czy domek o danych wspolrzednych w danej macierzy nie ma podlaczonego zbiornika, a ma tylko jedno niesprawdzone miejsce w sasiedztwie
--wtedy to miejsce na pewno musi byc zbiornikiem
isLastPlaceAvalibleHouse :: Int -> Int -> Matrix Integer -> Bool
isLastPlaceAvalibleHouse y x mat =	if hasTank y x mat 
					then False 
					else 	if (busyPlaces y x mat == (avaliblePlaces y x mat) - 1) 
						then True 
						else False
----------------------------------------
--sprawdza czy domek o danych wspolrzednych w danej macierzy ma podlaczony zbiornik niesprawdzajac, czy ten zbiornik sasiaduje z innym domkiem 
--(sytuacja gdy np jest w wierszu domek-zbiornik-domek-zbiornik) - wykonywane, gdy sprawdzanie z kontrola sasiedztwa nie daje nowych wynikow
hasTank y x mat = 	(safeGetMatrix (y-1) x mat == 3) ||
			(safeGetMatrix (y+1) x mat == 3) || 
			(safeGetMatrix y (x-1) mat == 3) || 
			(safeGetMatrix y (x+1) mat == 3)

neighbourTanksNum y x mat =  	let tanks = 	(safeGetMatrix (y-1) x mat == 3):
						(safeGetMatrix (y+1) x mat == 3): 
						(safeGetMatrix y (x-1) mat == 3): 
						(safeGetMatrix y (x+1) mat == 3):[]
				in countFreqList True tanks

getNeighbourTankPos y x mat  	|(safeGetMatrix (y-1) x mat == 3) = ((y-1), x)
				|(safeGetMatrix (y+1) x mat == 3) = ((y+1), x)
				|(safeGetMatrix y (x-1) mat == 3) = (y, (x-1))
				|(safeGetMatrix y (x+1) mat == 3) = (y, (x+1))
				|otherwise = (0,0)
				

hasOwnTank y x mat = 	let tankPos = getNeighbourTankPos y x mat
			in	if (neighbourTanksNum y x mat == 0)
				then False
				else 	if (neighbourTanksNum y x mat == 1)
					then 	if ((neighbourHouses (fst tankPos) (snd tankPos) mat) == 1)
						then True
						else False
					else 	if(neighbourTanksNum y x mat > 1)
						then True
						else False
				



avaliblePlaces y x mat = let 	left = 	x/=1
		                right = x/= ncols mat
		                top = y/=1
		                down = y/= nrows mat
				avalible = left:right:top:down:[]
			in	countFreqList True avalible

busyPlaces y x mat =    let 	checked = 	(safeGetMatrix (y-1) x mat == 9 || safeGetMatrix (y-1) x mat == 1 ):
						(safeGetMatrix (y+1) x mat == 9 || safeGetMatrix (y+1) x mat == 1 ):
						(safeGetMatrix y (x-1) mat == 9 || safeGetMatrix y (x-1) mat == 1 ):
						(safeGetMatrix y (x+1) mat == 9 || safeGetMatrix y (x+1) mat == 1 ):[]
			in 	countFreqList True checked

neighbourHouses y x mat =  	let 	checked = 	(safeGetMatrix (y-1) x mat == 1 ):
							(safeGetMatrix (y+1) x mat == 1 ):
							(safeGetMatrix y (x-1) mat == 1 ):
							(safeGetMatrix y (x+1) mat == 1 ):[]
				in 	countFreqList True checked

-- if house has tank
-- 	that tank neighbourHouses ==1
-- that house has own tank

-- if 


nearHouse y x mat = 	(safeGetMatrix (y-1) x mat == 1) ||  
			(safeGetMatrix (y-1) (x-1) mat == 1) ||  
			(safeGetMatrix (y-1) (x+1) mat == 1) ||  
			(safeGetMatrix (y+1) x mat == 1) || 
			(safeGetMatrix (y+1) (x-1) mat == 1) || 
			(safeGetMatrix (y+1) (x+1) mat == 1) || 
			(safeGetMatrix y (x-1) mat == 1) || 
			(safeGetMatrix y (x+1) mat == 1) 

----------------------------------------
--sprawdza czy domek o danych wspolrzednych w danej macierzy ma podlaczony zbiornik, z ktorym nie sasiaduje inny domek
--mozna wtedy uznac, ze ten domek ma juz podlaczony zbiornik
hasHouseTank :: Int -> Int -> Matrix Integer -> Bool
hasHouseTank y x mat = if ((hasTank y x mat) && not (nearHouse y x mat)) then True else False





checkAndReplace y x mat checkFun from to =   	if (checkFun y x mat) then
							if (safeGetMatrix (y-1) x mat  == from) then (setElem to (y-1,x) mat)
							else if (safeGetMatrix (y+1) x mat == from) then (setElem to (y+1,x) mat)
							else if (safeGetMatrix y (x-1) mat == from) then (setElem to (y,x-1) mat)
							else if (safeGetMatrix y (x+1) mat == from) then (setElem to (y,x+1) mat)
							else mat
						else mat


processHasHouseTank :: Int -> Int -> Matrix Integer -> Matrix Integer						
processHasHouseTank y x mat =  checkAndReplace y x mat hasOwnTank 0 9



agressiveProcessHasHouseTank :: Int -> Int -> Matrix Integer -> Matrix Integer
agressiveProcessHasHouseTank y x mat =   checkAndReplace y x mat hasTank 0 9


processIsLastPlaceAvalible :: Int -> Int -> Matrix Integer -> Matrix Integer						
processIsLastPlaceAvalible y x mat =  checkAndReplace y x mat isLastPlaceAvalibleHouse 0 3 

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
                                                                                then processElemFunInt (pos+1) (mapFun changeEmptyToTank (pos) mat) list getFun mapFun
                                                                                else processElemFunInt (pos+1) mat list getFun mapFun

checkNeighbours mat = checkNeighboursIterate 1 1 mat

checkHousesLastPlaces mat = checkHousesLastPlacesIterate 1 1 mat

checkHousesWithTanks mat = checkHousesWithTanksIterate 1 1 mat

agressiveCheckHousesWithTanks mat = agressiveCheckHousesWithTanksIterate 1 1 mat

parseEqualPlaces mat = parseEqualPlacesIterate 1 1 mat

iterateThrMatrix y x mat fun  	| x > Data.Vector.length (getRow 1 mat) = iterateThrMatrix (y+1) 1 mat fun
                                | y > Data.Vector.length (getCol 1 mat) = mat
                                | otherwise =   let elem = mat Data.Matrix.! (y,x)
                                              	  in fun y x mat elem

checkNeighboursIterator y x mat elem =	if elem == 0
                               		then    if (isThisPlacePossibleHouse y x mat && isThisPlacePossibleTank y x mat)
                                                then checkNeighboursIterate y (x+1) mat 
                                                else checkNeighboursIterate y (x+1) (setElem 9 (y, x) mat)
                                        else checkNeighboursIterate y (x+1) mat 

checkNeighboursIterate y x mat = iterateThrMatrix y x mat checkNeighboursIterator

checkHousesLastPlacesIterator y x mat elem = 		if elem == 1
                                                        then checkHousesLastPlacesIterate y (x+1) (processIsLastPlaceAvalible y x mat)   
                                                        else checkHousesLastPlacesIterate y (x+1) mat 

checkHousesLastPlacesIterate y x mat = iterateThrMatrix y x mat checkHousesLastPlacesIterator

checkHousesWithTanksIterator y x mat elem = 		if elem == 1
                                                        then checkHousesWithTanksIterate y (x+1) (processHasHouseTank y x mat)   
                                                        else checkHousesWithTanksIterate y (x+1) mat 

checkHousesWithTanksIterate  y x mat = iterateThrMatrix y x mat checkHousesWithTanksIterator


agressiveCheckHousesWithTanksIterator y x mat elem =    let tempMat = agressiveProcessHasHouseTank y x mat
                                                	in 	if elem == 1
                                                        	then 	if (tempMat == mat) 
									then agressiveCheckHousesWithTanksIterate y (x+1) mat 
									else tempMat
                                                        	else agressiveCheckHousesWithTanksIterate y (x+1) mat 
							

agressiveCheckHousesWithTanksIterate y x mat = iterateThrMatrix y x mat agressiveCheckHousesWithTanksIterator
                                                        
parseEqualPlacesIterate y x mat = iterateThrMatrix y x mat parseEqualPlacesIterator


parseEqualPlacesIterator y x mat elem = if elem == 0
                                        then  setElem 3 (y,x) mat
                                        else parseEqualPlacesIterate y (x+1) mat

matElem x mat = Prelude.elem x (Data.Matrix.toList mat)


solvePuzzles mat xList yList    | not (matElem 0 mat) = mat
                                | otherwise = 
				let 	tempMat = checkHousesWithTanks(checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) ) 
					tempMat2 = agressiveCheckHousesWithTanks(checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) )
					tempMat3 = parseEqualPlaces mat
                                        --solvePuzzles (checkSharedTanks(checkNeighbours (checkSharedTanks(processCols (checkSharedTanks (processRows mat yList)) xList)))) xList yList -- po kazdym wolaniu funkcji jednej z tych 3 funkcji dac inne iterateThrMatrix ktore sprawdza domki, i jesli maja w sasiedztwie zbiornik
                                        -- solvePuzzles (checkHousesWithTanks(checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) ) ) xList yList  
				in 	if (mat == tempMat2) 
					then solvePuzzles tempMat3 xList yList 
					else 	if (mat == tempMat) 
						then solvePuzzles tempMat2 xList yList 
						else solvePuzzles tempMat xList yList 
