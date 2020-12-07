{-# LANGUAGE GADTs #-}

module Main where

data Red
data Black

data Zero
data Succ n

data RBNode a c n where
    -- Leaf nodes are black
    Leaf  :: RBNode a Black (Succ Zero)
    -- Black nodes can have children of any colour
    Black :: a -> RBNode a cl n -> RBNode a cr n -> RBNode a Black (Succ n)
    -- Red nodes can only have black children
    Red   :: a -> RBNode a Black n -> RBNode a Black n -> RBNode a Red n

-- Root should be black
data RedBlack a = forall n . Tree (RBNode a Black n)

test :: RBNode Integer Black (Succ (Succ Zero))
test = Black 123 (Leaf) (Red 456 Leaf Leaf)
--test2 = Black 123 (Leaf) (Black 456 Leaf Leaf)

t = Tree test

main :: IO ()
main = return ()
