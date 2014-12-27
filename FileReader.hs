module FileReader where
import qualified Data.Text    as T
import qualified Data.Text.IO as Text

readGameFile fname = 
                    do
                     fmap T.lines (Text.readFile fname)
                    
----------------------------------------
replaceChar [] = []
replaceChar (x:xs) = 
                if x == ',' 
                  then ' ' : replaceChar xs 
                else if x == '[' 
                  then ' ' : replaceChar xs
                else if x == ']' 
                  then ' ' : replaceChar xs
                else x : replaceChar xs
                
getIntListFromString row = 
                          do 
                            let row1U = (T.unpack row)
                            let row1R = replaceChar row1U
                            map read $ words row1R :: [Int]
                        

getTupleListFromString row = do
                               let rowU = (T.unpack row) 
                               read rowU :: [(Int, Int)]