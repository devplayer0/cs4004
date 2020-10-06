import Control.Parallel

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = par nf (nf + fib (n-2) )
         where nf = fib (n-1)

main = print $ fib 40
