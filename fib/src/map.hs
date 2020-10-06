import Control.Parallel

force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force _ = ()

parallel_map :: (a -> b) -> [a] -> [b]
parallel_map _ [] = []
--parallel_map f (x:xs) = nm `par` (nm:parallel_map f xs)
--    where nm = f x
--parallel_map f (x:xs) = nr `par` (f x:nr)
--    where nr = parallel_map f xs
parallel_map f (x:xs) = nr `par` (nm:nr)
    where nm = f x
          nr = parallel_map f xs

l :: [Float]
l = [2..16000000]

main = print $ force $ parallel_map (* 2) l

{-
    `parallel_map` performs a map operation in parallel. It does this by using
    the `par` helper to indicate `nr` (the remaining values to be calculated)
    can be calculated in parallel. (ThreadScope results https://imgur.com/a/k4X06ni)
    The results show utilisation of all cores, although the execution time doesn't
    appear to be shorter... Choosing `nm` over `nr` to parallelise is much slower.

    `force` ensures that all of a list is evaluated through `pseq` (Haskell
    being lazy would not evauluate anything otherwise)
-}
