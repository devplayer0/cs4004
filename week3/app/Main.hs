{-# LANGUAGE InstanceSigs #-}

data List a = Nil
            | Cons a (List a)

-- combine two lists into a single one ([1, 2] ++ [3] -> [1, 2, 3])
lconcat :: List a -> List a -> List a
lconcat Nil xs = xs
lconcat (Cons x xs) ys = Cons x (lconcat xs ys)

-- reduce a list of lists to a single list ([[1, 2], [3]] -> [1, 2, 3])
join :: List (List a) -> List a
join Nil = Nil
join (Cons xs ys) = lconcat xs (join ys)

-- map every element in the list
instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure :: a -> List a
    pure x = Cons x Nil

    --(<*>) :: List (a -> b) -> List a -> List b
    --fs <*> xs =

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    xs >>= f = join $ fmap f xs

-- Left identity: return 123 >>= f === f 123
--     join (fmap f (Cons 123 Nil)) =
--     join (Cons (f 123) Nil) =
--     Cons (f 123) Nil

-- Right identity: Cons 123 Nil >>= return === Cons 123 Nil
--     join (fmap return (Cons 123 Nil)) =
--     join (Cons (return 123) Nil) =
--     join (Cons (Cons 123 Nil) Nil) =
--     Cons 123 Nil

-- Associativity: (Cons 123 Nil >>= f) >>= g === Cons 123 Nil >>= (\x -> f x >>= g)
--     Cons 123 Nil >>= f = Cons (f 123) Nil ->
--     Cons (f 123) Nil >>= g = Cons (g (f 123)) Nil
--
--     (\x -> f x) >>= g = join (fmap g (\x -> f x))


data Pair a b = P a b

-- Identity: fmap id (Pair () 123) = Pair () (id 123) = Pair () 123

-- Composition: fmap (f . g) (Pair () 123) =
--              Pair () ((f . g) 123) =
--              Pair () (f (g 123))
--
--              fmap f (Pair () 123) . fmap g (Pair () 123) =
--                  fmap f (fmap g (Pair () 123)) =
--                  fmap f (Pair () (g 123)) =
--                  Pair () (f (g 123))
instance Functor (Pair o) where
    fmap :: (a -> b) -> Pair o a -> Pair o b
    fmap f (P o x) = P o (f x)

-- impossible to give instance of Applicative since we don't have the left of the pair?
--instance Applicative (Pair o) where
--    pure :: a -> Pair o a
--    pure x = Pair ??? x

main = return ()
