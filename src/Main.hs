module Main where
 
import qualified MyLib
 
main :: IO ()
main = do 
     putStrLn "Program wypisuje ilosc slow z pliku"
     content <- readFile "test1000.txt"
     let result = MyLib.processFile content
     print result
