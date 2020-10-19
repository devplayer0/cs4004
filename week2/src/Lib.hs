module Lib
    ( someFunc
    ) where

type World = ()
type IO' a = World -> (a, World)

(>>==) :: IO' a -> (a -> IO' b) -> IO' b
(>>==) l r = \w -> let (x, w1) = l w in r x w1

(>>>) :: IO' a -> IO' b -> IO' b
(>>>) l r = l >>== (\_ -> r)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
