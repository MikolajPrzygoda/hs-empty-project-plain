module Student where

data Student = Student {
    firstName::String,
    lastName::String,
    age::Int
} deriving (Show, Eq)

s = Student "Ala" "ma" 3

fullName::Student->String
fullName student = firstName student ++ " " ++ lastName student

setAge::Student->Int->Student
setAge student newAge = Student (firstName student) (lastName student) newAge

initials::Student->String
initials (Student (ii:_) (il:_) _) = [ii] ++ ". " ++ [il] ++ "."

