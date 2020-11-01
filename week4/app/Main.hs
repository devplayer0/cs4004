{-# LANGUAGE InstanceSigs #-}
module Main where

--data Writer a = Journal [String] a
--    deriving Show
--
--instance Functor Writer where
--    fmap :: (a -> b) -> Writer a -> Writer b
--    fmap f (Journal es r) = Journal es (f r)
--
--instance Applicative Writer where
--    pure :: a -> Writer a
--    pure x = Journal [] x
--
--    (<*>) :: Writer (a -> b) -> Writer a -> Writer b
--    (Journal es f) <*> (Journal es2 r) = Journal (es ++ es2) (f r)
--
--instance Monad Writer where
--    (>>=) :: Writer a -> (a -> Writer b) -> Writer b
--    (Journal es ra) >>= f =
--        let (Journal es2 rb) = f ra
--        in Journal (es ++ es2) rb
--
--tell :: String -> Writer ()
--tell e = Journal [e] ()
--
--example :: Writer String Int
--example = do
--    tell "entry 1"
--    tell "entry 2"
--    return (1 + 1)

-- The problem is that we need to combine the entries from two different
-- `Journal`s, some sort of generically combinable type constraint ([] would
-- satisfy this)
data Writer l a = Journal [l] a
    deriving Show

instance Functor (Writer l) where
    fmap :: (a -> b) -> Writer l a -> Writer l b
    fmap f (Journal es r) = Journal es (f r)

instance Applicative (Writer l) where
    pure :: a -> Writer l a
    pure x = Journal [] x

    (<*>) :: Writer l (a -> b) -> Writer l a -> Writer l b
    (Journal es f) <*> (Journal es2 r) = Journal (es ++ es2) (f r)

instance Monad (Writer l) where
    (>>=) :: Writer l a -> (a -> Writer l b) -> Writer l b
    (Journal es ra) >>= f =
        let (Journal es2 rb) = f ra
        in Journal (es ++ es2) rb

tell :: l -> Writer l ()
tell e = Journal [e] ()

example :: Writer String Int
example = do
    tell "entry 1"
    tell "entry 2"
    return (1 + 1)

main :: IO ()
main = print example
