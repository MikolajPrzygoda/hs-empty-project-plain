module Tree where

data Tree a =
    Node {
        left::Tree a,
        right::Tree a,
        value::a }
    | Empty
    deriving (Show)

-- Test data
tree1 = Node Empty Empty 5
tree2 = Node (Node Empty (Node Empty Empty 4) 3) (Node Empty Empty 7) 5
tree3 = Node (Node Empty Empty 5) Empty 4
-- ========= 

merge :: Tree a -> Tree a -> a -> Tree a
merge leftTree rightTree rootValue = Node leftTree rightTree rootValue

remove Empty _ = Empty
remove (Node left right val) toRemove
 | val == toRemove = Empty
 | otherwise = merge (remove left toRemove) (remove right toRemove) val

tmap _ Empty = Empty
tmap func (Node left right val) = Node (tmap func left) (tmap func right) (func val)

nsum Empty = 0
nsum (Node left right val) = (nsum left) + val + (nsum right)

nnodes Empty = 0
nnodes (Node left right val) = (nnodes left) + 1 + (nnodes right)

leaves Empty = []
leaves (Node Empty Empty val) = [Node Empty Empty val]
leaves (Node left right _) = leaves(left) ++ leaves(right)


toString :: Show a => Tree a -> IO()
toString tree = do putStrLn $ toStringRec tree

toStringRec :: Show a => Tree a -> String
toStringRec Empty = ""
toStringRec (Node Empty Empty val) = show val
toStringRec (Node left right val) = 
    show val ++ ['('] ++ toStringRec left ++ [','] ++ toStringRec right ++ [')']

data TraverseMode = 
    VLR
    | LVR
    | LRV
    | VRL
    | RVL
    | RLV

traverse' :: TraverseMode -> Tree a -> [a]
traverse' _ Empty = []
traverse' VLR (Node l r val) = [val] ++ (traverse' VLR l) ++ (traverse' VLR r)
traverse' LVR (Node l r val) = (traverse' LVR l) ++ [val] ++ (traverse' LVR r)
traverse' LRV (Node l r val) = (traverse' LRV l) ++ (traverse' LRV r) ++ [val]
traverse' VRL (Node l r val) = [val] ++ (traverse' VRL r) ++ (traverse' VRL l)
traverse' RVL (Node l r val) = (traverse' RVL r) ++ (traverse' RVL l) ++ [val]
traverse' RLV (Node l r val) = (traverse' RLV r) ++ (traverse' RLV l) ++ [val]


