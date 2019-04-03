module Tree where

data Tree a =
    Node {
        left::Tree a,
        right::Tree a,
        value::a }
    | Empty
    deriving (Show)

tree1 = Node Empty Empty 5
tree2 = Node (Node Empty (Node Empty Empty 4) 3) (Node Empty Empty 7) 5
tree3 = Node (Node Empty Empty 5) Empty 4


merge :: Tree a -> Tree a -> a -> Tree a
merge leftTree rightTree rootValue = Node leftTree rightTree rootValue

remove Empty _ = Empty
remove (Node left right val) toRemove
 | val == toRemove = Empty
 | otherwise = merge (remove left toRemove) (remove right toRemove) val
