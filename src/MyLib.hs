module MyLib where
 
import qualified Data.Map as Map
import qualified Data.Heap as Heap

-- alias typu dla typu wynikowego
type Result = [(String,Int)]
type WordMap = Map.Map String Int

--glowna funkcja realizujaca zliczanie wyrazow dla calego pliku 
--przekazanego w formie pojedynczego lancucha znakow
--processFile :: String -> Result
--processFile fileContent = toResult . selectMostFrequent 10 . countWords . toWordList $ fileContent
processFile fileContent = countWords $ toWordList fileContent

-- Splits long String into [String] on ' '
toWordList :: String -> [String]
toWordList stringToSplit = list ++ [buffer]
    where
    (buffer, list) = foldl _splitter ("", []) stringToSplit

_splitter :: (String, [String]) -> Char -> (String, [String])
_splitter (buffer, list) c
    | c == ' ' = ("", list ++ [buffer])
    | c == '\n' = ("", list ++ [buffer])
    | otherwise = (buffer ++ [c], list)

-- Processes [String] into map containing number of 'key'::String occurances  
countWords :: [String] -> WordMap
countWords listOfWords = foldl _inserter Map.empty listOfWords

_inserter :: WordMap -> String -> WordMap
_inserter map string
    | Map.member string map = Map.insert string (map Map.! string + 1) map 
    | otherwise = Map.insert string 1 map

-- Add tuples from map into MinHead
selectMostFrequent :: Int -> WordMap -> Heap.MinHeap (String,Int)
selectMostFrequent n map = Heap.empty

toResult :: Heap.MinHeap (String,Int) -> Result
toResult heap = []

