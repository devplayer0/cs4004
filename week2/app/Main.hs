import System.IO

-- Do each of the actions, returning only the last one (blind operator used to
-- pass IO along)
f1 :: [IO a] -> IO a
f1 (x:[]) = x
f1 (x:xs) = x >> f1 xs


hello = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']

-- prints hello using `f1` as described
main2 = f1 actions
    where actions = hello


-- a and b remain thunks and are never actually evaluated (sugared syntax makes
-- this unclear)
-- Hi there is the only thing printed
main3 = do
         let a = f1 actions
             b = if True then putChar 'a' else putChar 'b'
         putStr "Hi there"
       where actions = hello

-- desugaring the do shows a and b never referenced in the `in`
main3a =
    let a = f1 actions
        b = if True then putChar 'a' else putChar 'b'
    in putStr "Hi there"
    where actions = hello


while :: IO Bool -> IO ()

-- do the action, storing the boolean and then either recurse on a again or
-- use return to give a an IO ()
while a = do
    ok <- a
    if ok then while a else return ()

-- desugared shows the bind and use of lambda to store `ok`
whilea a = a >>=
        \ok -> if ok then while a else return ()

-- keep prompting until the user enters "yes"
main4 = while $ do
    putStr "say yes: "
    hFlush stdout

    l <- getLine
    return $ l /= "yes"


f2 :: [IO a] -> IO [a]
f2 [] = return []

-- do x and store the result in y, then recurse and return the cons
f2 (x:xs) = do
    y <- x
    ys <- f2 xs
    return $ y:ys

-- desugaring shows the bindings
f2a (x:xs) = x >>=
        \y -> f2 xs >>=
            \ys -> return $ y:ys

read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions

main5 = do
    putStr "give me 10 chars: "
    hFlush stdout

    s <- read10
    putStr "you said: "
    putStrLn s

main = main5
