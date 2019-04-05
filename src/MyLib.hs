module MyLib where
 
import qualified Data.Map as Map
import qualified Data.Heap as Heap

-- alias typu dla typu wynikowego
type Result = [(Int,String)]
type WordMap = Map.Map String Int
type WordHeap = Heap.MaxPrioHeap Int String

--glowna funkcja realizujaca zliczanie wyrazow dla calego pliku 
--przekazanego w formie pojedynczego lancucha znakow
processFile :: String -> Result
processFile fileContent = toResult 10 . heapifyMap . mapWords . toWordList $ fileContent


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
mapWords :: [String] -> WordMap
mapWords listOfWords = foldl _mapper Map.empty listOfWords

_mapper :: WordMap -> String -> WordMap
_mapper map string
    | Map.member string map = Map.insert string (map Map.! string + 1) map 
    | otherwise = Map.insert string 1 map

-- Add tuples from map into MinHeap
--foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
heapifyMap :: WordMap -> WordHeap
heapifyMap map = Map.foldlWithKey _heapifier Heap.empty map

_heapifier :: WordHeap -> String -> Int -> WordHeap
_heapifier heap string n = Heap.insert (n, string) heap

-- Retrieves n top elements from heap
toResult :: Int -> WordHeap -> Result
toResult n heap = Heap.take n heap

