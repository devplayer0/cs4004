data Tree a = Leaf a
            | Node (Tree a) (Tree a)
    deriving Show


count :: Tree a -> Integer
count (Leaf _) = 1
count (Node l r) = count l + count r

depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node l r) = 1 + max (depth l) (depth r)

flatten :: Tree a -> [a]
flatten (Leaf v) = [v]
flatten (Node l r) = flatten l ++ flatten r

t1 = Node (Leaf 1) (Leaf 2)
t2 = Node (Node (Leaf 1) (Node (Node (Leaf 1) (Leaf 9)) (Leaf 7))) (Leaf 2)

main = print $ flatten t2
