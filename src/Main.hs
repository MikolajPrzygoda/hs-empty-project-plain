module Main where
 
import qualified Counter
 
main :: IO ()
main = do 
--  Zadanie 2 --
--     putStrLn "Program wypisuje ilosc slow z pliku"
--     content <- readFile "test100.txt"
--     let result = Counter.processString content
--     print result
----------------

--  Zadanie 3 --
    putStrLn "Program wypisuje ilość wystąpień 3-merów genomu e-coli."
    content <- readFile "res/ecoliOneEighth"
    print . Counter.count3Mers $ content
----------------
