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
mat = fromLists [ [0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1],[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0],[0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0],[1,0,0,0,1,0,1,1,0,0,0,1,0,0,0,0],[0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,0],[0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0],[0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1],[0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0],[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1],[0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0],[0,1,0,0,0,0,0,1,1,0,0,0,1,0,0,0],[0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1],[0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0],[0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0] ]
xList = [4,2,4,0,6,0,5,2,5,2,5,2,4,3,2,5]
yList = [4,3,2,3,4,2,6,1,6,1,4,2,6,0,4,3]


----------------------------------------
--funkcja uzywana zamiany listy Integerow na liste Intow w celu zapewnienia kompatybilnosci typow
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
--sprawdza czy domek o danych wspolrzednych w danej macierzy ma sasiadujacy zbiornik
hasTank y x mat = 	(safeGetMatrix (y-1) x mat == 3) ||
			(safeGetMatrix (y+1) x mat == 3) || 
			(safeGetMatrix y (x-1) mat == 3) || 
			(safeGetMatrix y (x+1) mat == 3)

----------------------------------------
--zwraca liczbe zbiornikow sasiadujacych z danym polem
neighbourTanksNum y x mat =  	let tanks = 	(safeGetMatrix (y-1) x mat == 3):
						(safeGetMatrix (y+1) x mat == 3): 
						(safeGetMatrix y (x-1) mat == 3): 
						(safeGetMatrix y (x+1) mat == 3):[]
				in countFreqList True tanks
----------------------------------------
--zwraca pozycje zbiornika sasiadujacego z danym polem
getNeighbourTankPos y x mat  	|(safeGetMatrix (y-1) x mat == 3) = ((y-1), x)
				|(safeGetMatrix (y+1) x mat == 3) = ((y+1), x)
				|(safeGetMatrix y (x-1) mat == 3) = (y, (x-1))
				|(safeGetMatrix y (x+1) mat == 3) = (y, (x+1))
				|otherwise = (0,0)
				
----------------------------------------
--sprawdza, czy domek na danym polu ma juz na pewno podlaczony zbiornik
--jesli nie ma zbiornika w sasiedztwie to nie ma 
--jesli jest jeden zbiornik w sasiedztwie, z ktorym sasiaduje tylko ten domek, to ma
--jesli w sasiedztwie sa dwa lub wiecej zbiornikow zakladamy, ze jeden z nich jest podlaczony
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
				


----------------------------------------
--zwraca liczbe pol, ktore sa w sasiedztwie (wyklucza pola poza plansza)
avaliblePlaces y x mat = let 	left = 	x/=1
		                right = x/= ncols mat
		                top = y/=1
		                down = y/= nrows mat
				avalible = left:right:top:down:[]
			in	countFreqList True avalible

----------------------------------------
--zwraca liczbe zajetych juz pol w sasiedztwie (juz wykluczonych, lub takich, na ktorych jest domek)
busyPlaces y x mat =    let 	checked = 	(safeGetMatrix (y-1) x mat == 9 || safeGetMatrix (y-1) x mat == 1 ):
						(safeGetMatrix (y+1) x mat == 9 || safeGetMatrix (y+1) x mat == 1 ):
						(safeGetMatrix y (x-1) mat == 9 || safeGetMatrix y (x-1) mat == 1 ):
						(safeGetMatrix y (x+1) mat == 9 || safeGetMatrix y (x+1) mat == 1 ):[]
			in 	countFreqList True checked

----------------------------------------
--zwraca liczbe domkow w sasiedztwie
neighbourHouses y x mat =  	let 	checked = 	(safeGetMatrix (y-1) x mat == 1 ):
							(safeGetMatrix (y+1) x mat == 1 ):
							(safeGetMatrix y (x-1) mat == 1 ):
							(safeGetMatrix y (x+1) mat == 1 ):[]
				in 	countFreqList True checked

----------------------------------------
--analizuje dane pole podana funkcja 'checkFun' i jesli ta funkcja zwroci True
--to zamienia wszystkie sasiadujace pola z wartosci 'from' na wartosc 'to'
checkAndReplace y x mat checkFun from to =   	if (checkFun y x mat) then
							if (safeGetMatrix (y-1) x mat  == from) then (setElem to (y-1,x) mat)
							else if (safeGetMatrix (y+1) x mat == from) then (setElem to (y+1,x) mat)
							else if (safeGetMatrix y (x-1) mat == from) then (setElem to (y,x-1) mat)
							else if (safeGetMatrix y (x+1) mat == from) then (setElem to (y,x+1) mat)
							else mat
						else mat

----------------------------------------
--jesli domek na danym polu ma na pewno podlaczony zbiornik zamienia wszystkie sasiadujace
--nieprzeanalizowane pola na przeanalizowane
processHasHouseTank :: Int -> Int -> Matrix Integer -> Matrix Integer						
processHasHouseTank y x mat =  checkAndReplace y x mat hasOwnTank 0 9

----------------------------------------
--jesli domek na danym polu ma na pewno podlaczony zbiornik zamienia wszystkie sasiadujace
--nieprzeanalizowane pola na przeanalizowane
agressiveProcessHasHouseTank :: Int -> Int -> Matrix Integer -> Matrix Integer
agressiveProcessHasHouseTank y x mat =   checkAndReplace y x mat hasTank 0 9

----------------------------------------
--jesli domek na danym polu ma tylko jedno nieprzeanalizowane miejsce w sasiedztwie to zamienia
--je na zbiornik
processIsLastPlaceAvalible :: Int -> Int -> Matrix Integer -> Matrix Integer						
processIsLastPlaceAvalible y x mat =  checkAndReplace y x mat isLastPlaceAvalibleHouse 0 3 

----------------------------------------
--sprawdza, czy dane miejce moze byc potencjalnie zbiornikiem, czy
--wyklucza je bliskosc innego zbiornika
isThisPlacePossibleTank :: Int -> Int -> Matrix Integer -> Bool
isThisPlacePossibleTank y x mat = 	not ( (safeGetMatrix (y-1) x mat == 3) ||  
                                        (safeGetMatrix (y-1) (x-1) mat == 3) ||  
                                        (safeGetMatrix (y-1) (x+1) mat == 3) ||  
                                        (safeGetMatrix (y+1) x mat == 3) || 
                                        (safeGetMatrix (y+1) (x-1) mat == 3) || 
                                        (safeGetMatrix (y+1) (x+1) mat == 3) || 
                                        (safeGetMatrix y (x-1) mat == 3) || 
                                        (safeGetMatrix y (x+1) mat == 3) )

----------------------------------------
--sprawdza, czy dane miejce moze byc potencjalnie zbiornikiem, bo sasiaduje z domkiem
safeGetMatrix :: Int -> Int -> Matrix Integer -> Integer
safeGetMatrix y x mat	| x<1 = 8
			| y<1 = 8
			| x>ncols mat = 8
			| y>nrows mat = 8
			| otherwise = mat Data.Matrix.!	(y,x)

----------------------------------------
--sprawdza ile jest danych elementow w danym wektorze
countFreq elem vec = Data.Vector.length (Data.Vector.filter (\x -> x==elem) vec)

----------------------------------------
--sprawdza ile jest danych elementow w danej liscie
countFreqList elem list = Data.List.length (Data.List.filter (\x -> x==elem) list)

----------------------------------------
-- przetwarza liczby zbiornikow w danych wierszach, zapisane na brzegach planszy
-- dokladniejszy opis w opisie funkcji processElemFunInt
processRows mat list = processElemFunInt 1 mat (listFromIntegral list) getRow mapRow

----------------------------------------
-- w skrocie - przetwarza liczby zbiornikow w danych kolumnach, zapisane na brzegach planszy
-- dokladniejszy opis w opisie funkcji processElemFunInt
processCols mat list = processElemFunInt 1 mat (listFromIntegral list) getCol mapCol

----------------------------------------
-- w skrocie - przetwarza liczby zbiornikow w danych liniach, zapisane na brzegach planszy
--
-- -jesli liczba zbiornikow w danej linii jest rowna docelowej liczbie zbiornikow w tej linii, okreslonej
-- 	liczba na brzegu planszy, zamienia pozostale nieprzetworzone miejsca na przetworzone
-- -jesli liczba wolnych miejsc powiekszona o liczbe zbiornikow w tej linii jest mniejsza lub rowna 
-- 	docelowej liczbie zbiornikow w tej linii okreslonej liczba na brzegu planszy, zamienia
-- 	pozostale wolne miejsca na zbiorniki
-- 
-- rozpoczyna dzialanie w pos i wywoluje sie sama dla kolejnych pozycji, az dojdzie do konca listy
-- list - lista liczb okreslajacych liczbe zbiornikow w danej linii
-- getFun i mapFun - funkcja pobierajaca i mapujaca na wiersz lub kolumne w macierzy - w zaleznosci od wymiaru
-- 	w ktorym ma byc sprawdzenie
processElemFunInt pos mat list getFun mapFun    | pos >= Prelude.length list    = mat
                                                | otherwise =
                                                        let numTanks = list !! (pos-1)
                                                        in 
                                                                if countFreq 3 (getFun pos mat) == numTanks
                                                                then processElemFunInt (pos+1) (mapFun changeEmptyToChecked (pos) mat) list getFun mapFun
                                                                else    if ( countFreq 0 (getFun pos mat) + countFreq 3 (getFun pos mat) ) <= numTanks
                                                                        then processElemFunInt (pos+1) (mapFun changeEmptyToTank (pos) mat) list getFun mapFun
                                                                        else processElemFunInt (pos+1) mat list getFun mapFun
----------------------------------------
--eliminuje nieprzetworzone pola wykluczane przez sasiedztwo zbiornika
--i brak sasiedztwa domkow
checkNeighbours mat = checkNeighboursIterate 1 1 mat

----------------------------------------
--wstawia zbiorniki przy domkach, w sasiedztwie ktorych zostalo tylko jedno wolne pole
checkHousesLastPlaces mat = checkHousesLastPlacesIterate 1 1 mat

----------------------------------------
--eliminuje nieprzetworzone pola wokol domkow, ktore maja
--juz podlaczony zbiornik
checkHousesWithTanks mat = checkHousesWithTanksIterate 1 1 mat

agressiveCheckHousesWithTanks mat = agressiveCheckHousesWithTanksIterate 1 1 mat

----------------------------------------
--rozpatruje przypadek, gdy pozostale wolne miejsca sa rownowazne dla algorytmu, zamienia pierwsze napotkane 
--na zbiornik i zwraca taka macierz do sprawdzenia, czy przez te zmiany zmienily sie przewidywania algorytmu
--dla pozostalych pol
parseEqualPlaces mat = parseEqualPlacesIterate 1 1 mat

----------------------------------------
--aplikuje podana funkcje 'fun' do wszystkich kolejnych pol macierzy poczynajac od podanego
iterateThrMatrix y x mat fun  	| x > Data.Vector.length (getRow 1 mat) = iterateThrMatrix (y+1) 1 mat fun
                                | y > Data.Vector.length (getCol 1 mat) = mat
                                | otherwise =   let elem = mat Data.Matrix.! (y,x)
                                              	  in fun y x mat elem

----------------------------------------
--przeznacozna do zaaplikowania do wszystkich pol macierzy
--
--eliminuje nieprzetworzone pola wykluczane przez sasiedztwo zbiornika
--i brak sasiedztwa domku
checkNeighboursIterator y x mat elem =	if elem == 0
                               		then    if (isThisPlacePossibleHouse y x mat && isThisPlacePossibleTank y x mat)
                                                then checkNeighboursIterate y (x+1) mat 
                                                else checkNeighboursIterate y (x+1) (setElem 9 (y, x) mat)
                                        else checkNeighboursIterate y (x+1) mat 

----------------------------------------
--uruchamia aplikowanie do wszystkich kolejnych pol macierzy poczynajac od podanego funkcji, ktora robi to, co opisano ponizej
--
--eliminuje nieprzetworzone pola wykluczane przez sasiedztwo zbiornika
--i brak sasiedztwa domku
checkNeighboursIterate y x mat = iterateThrMatrix y x mat checkNeighboursIterator

----------------------------------------
--przeznacozna do zaaplikowania do wszystkich pol macierzy
--
--wstawia zbiorniki przy domkach, w sasiedztwie ktorych zostalo tylko jedno wolne pole
checkHousesLastPlacesIterator y x mat elem = 	if elem == 1
                                                then checkHousesLastPlacesIterate y (x+1) (processIsLastPlaceAvalible y x mat)   
                                                else checkHousesLastPlacesIterate y (x+1) mat 

----------------------------------------
--uruchamia aplikowanie do wszystkich kolejnych pol macierzy poczynajac od podanego funkcji, ktora robi to, co opisano ponizej
--
--wstawia zbiorniki przy domkach, w sasiedztwie ktorych zostalo tylko jedno wolne pole
checkHousesLastPlacesIterate y x mat = iterateThrMatrix y x mat checkHousesLastPlacesIterator

----------------------------------------
--przeznacozna do zaaplikowania do wszystkich pol macierzy
--
--eliminuje nieprzetworzone pola wokol domkow, ktore maja
--juz podlaczony zbiornik
checkHousesWithTanksIterator y x mat elem = 	if elem == 1
                                                then checkHousesWithTanksIterate y (x+1) (processHasHouseTank y x mat)   
                                                else checkHousesWithTanksIterate y (x+1) mat 

----------------------------------------
--uruchamia aplikowanie do wszystkich kolejnych pol macierzy poczynajac od podanego funkcji, ktora robi to, co opisano ponizej
--
--eliminuje nieprzetworzone pola wokol domkow, ktore maja
--juz podlaczony zbiornik
checkHousesWithTanksIterate  y x mat = iterateThrMatrix y x mat checkHousesWithTanksIterator


agressiveCheckHousesWithTanksIterator y x mat elem =    let tempMat = agressiveProcessHasHouseTank y x mat
                                                	in 	if elem == 1
                                                        	then 	if (tempMat == mat) 
									then agressiveCheckHousesWithTanksIterate y (x+1) mat 
									else tempMat
                                                        	else agressiveCheckHousesWithTanksIterate y (x+1) mat 
							
agressiveCheckHousesWithTanksIterate y x mat = iterateThrMatrix y x mat agressiveCheckHousesWithTanksIterator
                                                        
----------------------------------------
--przeznacozna do zaaplikowania do wszystkich pol macierzy
--
--rozpatruje przypadek, gdy pozostale wolne miejsca sa rownowazne dla algorytmu, zamienia pierwsze napotkane 
--na zbiornik i zwraca taka macierz do sprawdzenia, czy przez te zmiany zmienily sie przewidywania algorytmu
--dla pozostalych pol
parseEqualPlacesIterator y x mat elem = if elem == 0
                                        then  setElem 3 (y,x) mat
                                        else parseEqualPlacesIterate y (x+1) mat

----------------------------------------
--uruchamia aplikowanie do wszystkich kolejnych pol macierzy poczynajac od podanego funkcji, ktora robi to, co opisano ponizej
--
--rozpatruje przypadek, gdy pozostale wolne miejsca sa rownowazne dla algorytmu, zamienia pierwsze napotkane 
--na zbiornik i zwraca taka macierz do sprawdzenia, czy przez te zmiany zmienily sie przewidywania algorytmu
--dla pozostalych pol
parseEqualPlacesIterate y x mat = iterateThrMatrix y x mat parseEqualPlacesIterator

----------------------------------------
--sprawdza, czy dany element wystepuje w danej macierzy
matElem x mat = Prelude.elem x (Data.Matrix.toList mat)

----------------------------------------
--rozwiazuje zagadke dana macierza i listami wokol planszy w nastepujacych krokach
--
-- -przetwarza liczby zbiornikow w danych wierszach i kolumnach zapisane na brzegach planszy
-- 	dokladniejszy opis w opisie funkcji processElemFunInt
-- -eliminuje nieprzetworzone pola wykluczane przez sasiedztwo zbiornika
--	i brak sasiedztwa domkow
-- -wstawia zbiorniki przy domkach, w sasiedztwie ktorych zostalo tylko jedno wolne pole
-- -eliminuje nieprzetworzone pola wokol domkow, ktore maja
--	juz podlaczony zbiornik
-- -gdy napotka sytuacje, w ktorej pozostale wolne miejsca sa rownowazne dla algorytmu, zamienia pierwsze napotkane 
--	na zbiornik i zwraca taka macierz do sprawdzenia, czy przez te zmiany zmienily sie przewidywania algorytmu
--	dla pozostalych pol
-- -wykonuje te kroki w petli az plansza nie bedzie zawierac nieprzetworzonych pol
solvePuzzles mat xList yList    | not (matElem 0 mat) = mat
                                | otherwise = 
					let 	tempMat = checkHousesWithTanks(checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) ) 
						tempMat2 = agressiveCheckHousesWithTanks(checkHousesLastPlaces(checkNeighbours (processCols (processRows mat yList) xList)) )
						tempMat3 = parseEqualPlaces mat
					in 	if (mat == tempMat) --(mat == tempMat2) 
						then solvePuzzles tempMat3 xList yList 
						else 	if (mat == tempMat) 
							then solvePuzzles tempMat2 xList yList 
							else solvePuzzles tempMat xList yList 
