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
initials s 

-- Lab 3 = Laboratorium: Przetwarzanie danych, funkcje wyższego rzędu

listToProcess = [
    Student "Alicja" "Akla" 21,
    Student "Batrek" "Bodo" 20,
    Student "Celina" "Czyzyk" 21,
    Student "Damian" "Dab"  22,
    Student "Eustachy" "Elo" 20]

-- 1.1 - Utworzyć listę zawierającą pełne imiona i nazwiska studentów 
--       w postaci łańcuchów znaków.

fullNames :: [Student] -> [String]
fullNames [] = []
--fullNames (x:xs) = fullName x : fullNames xs
fullNames list = map fullName list

-- 1.2 - Utworzyć listę zawierającą pary w postaci krotek: 
--       numer porządkowy, student.
type OrderedStudent = (Int, Student)
orderedListOperation :: [OrderedStudent] -> Student -> [OrderedStudent]
orderedListOperation [] student = [(1, student)]
orderedListOperation accList student = accList ++ [(lastNum + 1, student)]
    where
    (lastNum, _) = last accList

orderedList :: [Student] -> [OrderedStudent]
orderedList list = foldl orderedListOperation [] list

-- 1.3 - Przetworzyć listę z powyższego punktu na raport tesktowy w formacie 
--       "1. student: Nazwisko I. wiek: Wiek"
processRow :: OrderedStudent -> String
processRow (lp, student) = 
    (show lp) ++ ". student: " ++ lastName student ++ " " ++ 
    [initial] ++ ". wiek: " ++ show (age student) ++ "\n"
    where
    initial:_ = firstName student
    
genRaportString :: [OrderedStudent] -> String
genRaportString list = foldl (++) "" (map processRow list)

{- 1.4 - Wygenerować z tabelkę HTML.
    Tabelka postaci:    
    | Lp. | Student     | Wiek |
    | 1.  | Nazwisko I. | 20   |
-}
processRowHTML :: OrderedStudent -> String
processRowHTML (poz, student) = 
    "<tr><td>" ++ show poz ++ ".</td><td>" ++ lastName student ++ " " ++
    [inicial] ++ ".</td><td>" ++ show (age student) ++ "</td></tr>"
    where
    (inicial:_) = firstName student

genRaportHTML :: [OrderedStudent] -> String
genRaportHTML list = 
    "<table>" ++ (foldl (++) "" (map processRowHTML list)) ++ "</table>"
    
-- 1.5 - Wygenerować listę zmian w postaci typu wydarzenia, 
--       StudentsFirstNameChangeEvent oldName newName, przez utworzenie
--       zmodyfikowanej listy studentów, a następnie porównanie. (???)
{-
data StudentChangeEvent = 
    StudentsFirstNameChangeEvent {
        oldName::String,
        newName::String 
    } | StudentsLastNameChangeEvent {
        oldName::String,
        newName::String
    }

isFirstNameChange :: StudentsChangeEvent -> Bool
isFirstNameChange (StudentsFirstNameChange _ _) = True
isFirstNameChange _ = False

isLastNameChange :: StudentsChangeEvent -> Bool
isLastNameChange (StudentsLastNameChange _ _) = True
isLastNameChange _ = False

 
applyEvent :: [Student] -> StudentChangeEvent -> [Student]
applyEvent list event
    | isFirstNameChange event = changeFirstName list event
    | isLastNameChange event = changeLastName list event
-}

  










