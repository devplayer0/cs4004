import Control.Parallel

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = nf1 `par` nf2 `pseq` nf1 + nf2
        where nf1 = fib (n-1)
              nf2 = fib (n-2)

main = print $ fib 37
