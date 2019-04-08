module Counter where
 
import qualified Data.Map as Map
import qualified Data.Heap as Heap

import Data.Maybe as Maybe

type Result = [(Int,String)]
type WordMap = Map.Map String Int
type WordHeap = Heap.MaxPrioHeap Int String

-- Counts each String occurance and sorts the list using heap then returns
-- n most common ones.
findNMostFrequent :: Int -> [String] -> Result
findNMostFrequent n list = toResult n . heapifyMap . mapWords $ list


-- Splits long String into [String] on any character in Just [Char],
-- if Nothing is supplied splits on ' ' and '\n'
toWordList :: String -> Maybe [Char] -> [String]
toWordList stringToSplit delims = list ++ [buffer]
    where
    _delims = Maybe.fromMaybe " \n" delims
    (buffer, list, _) = foldl _splitter ("", [], _delims) stringToSplit

type SplitterStruct = (String, [String], [Char])
_splitter :: SplitterStruct -> Char -> SplitterStruct
_splitter (buffer, list, delims) c
    | elem c delims = ("", list ++ [buffer], delims)
    | otherwise = (buffer ++ [c], list, delims)


-- Processes [String] into map containing number of 'key'::String occurances  
mapWords :: [String] -> WordMap
mapWords listOfWords = foldl _mapper Map.empty listOfWords

_mapper :: WordMap -> String -> WordMap
_mapper map string = 
    Map.insert string (Maybe.fromMaybe 0 (Map.lookup string map) + 1) map 


-- Add tuples from map into MinHeap
--foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
heapifyMap :: WordMap -> WordHeap
heapifyMap map = Map.foldlWithKey _heapifier Heap.empty map

_heapifier :: WordHeap -> String -> Int -> WordHeap
_heapifier heap string n = Heap.insert (n, string) heap


-- Retrieves n top elements from heap
toResult :: Int -> WordHeap -> Result
toResult n heap = Heap.take n heap



-- Zad 2 - Find 10 most common words separated with " \n" in the string
processString :: String -> Result
processString string = 
    toResult 10 . heapifyMap . mapWords $ toWordList string Nothing


-- Zad 3 - Count the number of all the 3-meres occurences
to3Mers :: String -> [String]
to3Mers (x1:x2:x3:xs) = list ++ [buffer]
    where
    (buffer, list) = foldl _extractor (x1:x2:x3:[], []) xs

_extractor :: (String, [String]) -> Char -> (String, [String])
_extractor (buffer, list) c
    | c == '\n' = (buffer, list) -- ignore new line characters
    | otherwise = (drop 1 buffer ++ [c], list ++ [buffer])


count3Mers :: String -> Result
count3Mers string = toResult 64 . heapifyMap . mapWords $ (to3Mers string)





