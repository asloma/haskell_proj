module FileReader where

import System.IO
import System.IO.Error
import Control.Exception

----------------------------------------
--wszytuje plik gry
readGameFile fname =
                    catch (do 
                             contents <- readFile fname
                             return contents
                          ) errorHandler
                          where
                            errorHandler e =
                              if isDoesNotExistError e
                                then error ("Nie istnieje plik: " ++ fname)
                              else error ("Inny wyjatek")
                                        
----------------------------------------
--zwraca listê zawierajaca wiersze 1,2,3 z wczytanego pliku
getContentList content = 
                   do
                        let linia1String = takeWhile (/= '\n') content
                        --putStrLn ("linia1String " ++ linia1String)                       
                        let linia23String = dropWhile (=='\n') (dropWhile (/= '\n') content)
                        --putStrLn ("linia23String " ++ linia23String)
                        let linia2String = takeWhile (/= '\n') linia23String
                        --putStrLn ("linia2String " ++ linia2String)
                        let linia3String = takeWhile (/= '\n')(dropWhile (=='\n') (dropWhile (/= '\n') linia23String))
                        --putStrLn ("linia3String " ++ linia3String)
                        return [linia1String,linia2String,linia3String]
                     
----------------------------------------
--sprawdza czy wiersz (String) mo¿na rzutowaæ na [Integer]
isIntegerList s = case reads s :: [([Integer], String)] of
                        [(_, "")] -> putStr ""
                        _         -> error "Zly format pliku wejsciowego"

----------------------------------------
--sprawdza czy wiersz (String) mo¿na rzutowaæ na [(Int, Int)]      
isIntTopleList s = case reads s :: [([(Int,Int)], String)] of
                        [(_, "")] -> putStr ""
                        _         -> error "Zly format pliku wejsciowego"                     
           
----------------------------------------
--sprawdza d³ugoœæ String i zwraca ewentualny b³ad
isEmptyString::String->String->IO()
isEmptyString testString e = if length testString == 0
                                    then error e                  
                             else putStr ""
