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
                                then return ("Nie istnieje " ++ fname)
                              else return ("Inny wyjatek")
                                        
----------------------------------------
--zwraca listê zawierajaca wiersze 1,2,3 z wczytanego pliku
getContentList content = 
                   do
                     let linia1String = takeWhile (/= '\n') content
                     let linia23String = tail (dropWhile (/= '\n') content)
                     let linia2String = takeWhile (/= '\n') linia23String
                     let linia3String = tail (dropWhile (/= '\n') linia23String)
                     return [linia1String,linia2String,linia3String]
----------------------------------------
                     
           
                   
                
