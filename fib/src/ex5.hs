import Control.Parallel

-- Sequential version of fib, when we want to parallelism
sfib :: Integer -> Integer
sfib n | n < 2 = 1
sfib n = sfib (n-1) + sfib (n-2)

fib :: Integer -> Integer -> Integer
fib 0 n = sfib n
fib _ n | n < 2 = 1
fib d n = par nf1 (pseq nf2 (nf1 + nf2))
         where nf1 = fib (d-1) (n-1)
               nf2 = fib (d-1) (n-2)

main = print $ fib 3 37
