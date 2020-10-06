import Control.Parallel

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = par nf1 (pseq nf2 (nf1 + nf2))
        where nf1 = fib (n-1)
              nf2 = fib (n-2)

main = print $ fib 37
