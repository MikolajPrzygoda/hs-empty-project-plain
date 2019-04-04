module Student where

data Student = Student {
    firstName::String,
    lastName::String,
    age::Int
} deriving (Show, Read, Eq)

s = Student "Ala" "ma" 3

fullName::Student->String
fullName student = firstName student ++ " " ++ lastName student

setAge::Student->Int->Student
setAge student newAge = Student (firstName student) (lastName student) newAge

initials::Student->String
initials (Student (ii:_) (il:_) _) = [ii] ++ ". " ++ [il] ++ "."


-- Lab 3 = Laboratorium: Przetwarzanie danych, funkcje wyższego rzędu

listToProcess = [
    Student "Alicja" "Akla" 21,
    Student "Batrek" "Bodo" 20,
    Student "Celina" "Czyzyk" 21,
    Student "Damian" "Dab"  22,
    Student "Eustachy" "Elo" 20]

-- 1.1
fullNames :: [Student] -> [String]
fullNames [] = []
--fullNames (x:xs) = fullName x : fullNames xs
fullNames list = map fullName list

-- 1.2
type OrderedStudents = [(Int, Student)]
type OrderedStudent = (Int, Student)
orderedListOperation :: OrderedStudents -> Student -> OrderedStudents
orderedListOperation [] student = [(1, student)]
orderedListOperation accList student = accList ++ [(lastNum + 1, student)]
    where
    (lastNum, _) = last accList

orderedList :: [Student] -> OrderedStudents
orderedList list = foldl orderedListOperation [] list

--1.3
--raport :: OrderedStudents -> String
--raport (x:xs) = 

processRow :: OrderedStudent -> String
processRow (lp, student) = 
    (show lp) ++ ". student: " ++ lastName student ++ " " ++ 
    [initial] ++ ". wiek: " ++ show (age student) ++ "\n"
    where
    initial:_ = firstName student
    
    
    
    
    
    
    

