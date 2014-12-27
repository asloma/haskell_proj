module BoardOperator where
import Data.List

printRow rowName elem = putStrLn ((show rowName) ++ ":" ++ " " ++ concat (intersperse " "  elem))

printTable rowsNameList rowElemList = 
                                     mapM_ (uncurry printRow) (zip rowsNameList rowElemList)

printBoard  rowsNameList colsNameList table =
                                             do
                                               putStrLn ("------------------------------")
                                               putStrLn ("   " ++ concat (intersperse ": " (map show colsNameList))++ ":") 
                                               printTable rowsNameList table
                                               putStrLn ("Legenda: ")
                                               putStrLn ("hh - domek ")
                                               putStrLn ("ee - pole puste")
                                               putStrLn ("bb - pole zablokowane (nie bedzie zbiornika)")
                                               putStrLn ("tt - zbiornik gorny")
                                               putStrLn ("td - zbiornik dolny")
                                               putStrLn ("tl - zbiornik lewy")
                                               putStrLn ("tr - zbiornik prawy")
                                               putStrLn ("------------------------------")

changeElem table r c new = before ++ ((safeReplaceElement (table !! r) c new) : after)
                             where before = take r table
                                   after  = drop (r+1) table

safeReplaceElement xs i x =
                           if i >= 0 && i < length xs
                             then replaceElement xs i x
                           else xs

replaceElement xs i x = before ++ (x : after)
                          where before = take i xs
                                after  = drop (i+1) xs
        
