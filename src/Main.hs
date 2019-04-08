module Main where
 
import qualified Counter
 
main :: IO ()
main = do 
     putStrLn "Program wypisuje ilosc slow z pliku"
     content <- readFile "test100.txt"
     let result = Counter.processString content
     print result
