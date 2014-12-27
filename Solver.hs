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

mat = fromLists [ [0,1,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,1,0,1,0],[1,0,0,0,1,0],[0,0,1,0,0,1] ]
xList = [1,1,2,1,1,1]
yList = [1,0,2,1,2,1]

listFromIntegral list = Data.List.map fromIntegral list

sizesAreCorrect mat xList yList = 	if Data.Vector.length (getRow 1 mat) == Prelude.length xList && Data.Vector.length (getCol 1 mat) == Prelude.length yList then 
						True 
					else 	False

changeEmptyToChecked _ x =	if x == 0 then
				9
			else x

changeZeroToTank _ x =	if x == 0 then
				3 
			else x

isThisPlacePossibleHouse x y mat = 	let 	left = x/=1
						right = x/= ncols mat
						top = y/=1
						down = y/= nrows mat
					in 	(top && mat Data.Matrix.! (x,y-1) == 1) || (down && mat Data.Matrix.! (x,y+1) == 1) || (left && mat Data.Matrix.! (x-1,y) == 1) || (right && mat Data.Matrix.! (x+1,y) == 1)

isThisPlacePossibleTank x y mat = 	let 	left = x/=1
						right = x/= ncols mat
						top = y/=1
						down = y/= nrows mat
					in 	not ( (top && mat Data.Matrix.! (x,y-1) == 3) ||  
						(top && left && mat Data.Matrix.! (x-1,y-1) == 3) ||  
						(top && right && mat Data.Matrix.! (x+1,y-1) == 3) ||  
						(down && mat Data.Matrix.! (x,y+1) == 3) || 
						(down && left && mat Data.Matrix.! (x-1,y+1) == 3) || 
						(down && right && mat Data.Matrix.! (x+1,y+1) == 3) || 
						(left && mat Data.Matrix.! (x-1,y) == 3) || 
						(right && mat Data.Matrix.! (x+1,y) == 3) )

countFreq elem vec = Data.Vector.length (Data.Vector.filter (\x -> x==elem) vec)

processRows mat list = processElemFunInt 1 mat (listFromIntegral list) getRow mapRow

processCols mat list = processElemFunInt 1 mat (listFromIntegral list) getCol mapCol

--po wierszach, pionowa lista z numerami
processElemFunInt pos mat list getFun mapFun	| pos >= Prelude.length list 	= mat
							| otherwise =
								let numTanks = list !! (pos-1)
								in 
									if countFreq 3 (getFun pos mat) == numTanks
									then processElemFunInt (pos+1) (mapFun changeEmptyToChecked (pos) mat) list getFun mapFun
									else 	if ( countFreq 0 (getFun pos mat) + countFreq 3 (getFun pos mat) ) <= numTanks
										then processElemFunInt (pos+1) (mapFun changeZeroToTank (pos) mat) list getFun mapFun
										else processElemFunInt (pos+1) mat list getFun mapFun

checkNeighbours mat = iterateThrMatrix 1 1 mat

iterateThrMatrix posX posY mat 	| posX > Data.Vector.length (getRow 1 mat) = iterateThrMatrix 1 (posY+1) mat
				| posY > Data.Vector.length (getCol 1 mat) = mat
				| otherwise = 	let elem = mat Data.Matrix.! (posX, posY)
						in
							if elem == 0
							then 	if (isThisPlacePossibleHouse posX posY mat && isThisPlacePossibleTank posX posY mat)
								then iterateThrMatrix (posX+1) posY mat 
								else iterateThrMatrix (posX+1) posY (setElem 9 (posX, posY) mat)
							else iterateThrMatrix (posX+1) posY mat 


matElem x mat = Prelude.elem x (Data.Matrix.toList mat)

solvePuzzles mat xList yList 	| not (matElem 0 mat) = mat
				| otherwise = 
					--solvePuzzles (checkSharedTanks(checkNeighbours (checkSharedTanks(processCols (checkSharedTanks (processRows mat yList)) xList)))) xList yList -- po kazdym wolaniu funkcji jednej z tych 3 funkcji dac inne iterateThrMatrix ktore sprawdza domki, i jesli maja w sasiedztwie zbiornik
					solvePuzzles (checkNeighbours (processCols (processRows mat yList) xList)) xList yList	
					


