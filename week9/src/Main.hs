module Main where

import Control.Exception
import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import System.IO

-- https://stackoverflow.com/questions/21276844/prime-factors-in-haskell
primeFactors :: Integral n => n -> [n]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

data Choice = Work Int
            | Result
            | Quit

menu :: IO Choice
menu = do
  putStrLn "You can:"
  putStrLn "[\\d+]    Enter a number to start finding its prime factors"
  putStrLn "[result] Get the first available set of prime factors"
  putStrLn "[quit]   Exit the program"
  putStr   "What to do? "

  choiceStr <- getLine
  return $ case choiceStr of
    "result" -> Result
    "quit"   -> Quit
    n        -> Work $ read n

data WorkResult = WorkResult { n :: Int, factors :: [Int] }

worker :: Chan Int -> Chan WorkResult -> IO ()
worker i o = do
  forever $ do
    item <- readChan i
    factors <- evaluate $ force $ primeFactors item

    writeChan o $ WorkResult item factors

mainLoop i o = do
  choice <- menu
  case choice of
    Work n -> do
      writeChan i n
      putStrLn "submitted"
      mainLoop i o
    Result -> do
      result <- readChan o
      putStrLn $ "Prime factors of " ++ (show $ n result) ++ ": " ++ (show $ factors result)
      mainLoop i o
    Quit -> return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  i <- newChan
  o <- newChan
  replicateM 4 $ (forkOS (worker i o))

  mainLoop i o
