import Control.Parallel

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = par nf ( fib (n-1) + nf )
          where nf = fib (n-2)

main = print $ fib 40
