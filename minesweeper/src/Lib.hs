{-# LANGUAGE TemplateHaskell, MultiWayIf, TypeSynonymInstances, FlexibleInstances #-}
module Lib where

import qualified Data.Set as Set
import Control.Monad.State
import Control.Lens

import CSP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data SquareState =
    Covered
  | Mine
  | Uncovered Int

instance Show SquareState where
  show t = case t of
    Covered -> "#"
    Mine -> "*"
    Uncovered n -> if n == 0
      then " "
      else show n

printRow :: [SquareState] -> String
printRow [] = ""
printRow (s:ss) = show s ++ printRow ss

printSquares :: [[SquareState]] -> String
printSquares [] = ""
printSquares (ss:sss) = printRow ss ++ "\n" ++ printSquares sss

data Board = Board {
  _bounds :: Bounds
, _squares :: [[SquareState]]
, _csp :: Minesweeper
} deriving (Show)
makeLenses ''Board

printBoard :: Board -> IO ()
printBoard b = putStr $ printSquares $ b^.squares

squareAt :: Board -> Pos -> SquareState
squareAt b (x, y) = (b^.squares.element x) !! y

setSquare :: Pos -> SquareState -> Board -> Board
setSquare (x, y) v = squares.ix x.ix y .~ v

neighbouringMines :: Board -> Pos -> Int
neighbouringMines b p =
  Set.foldr' reducer 0 ns
  where
    reducer p =
      (+) $ case squareAt b p of
       Mine -> 1
       _ -> 0
    ns = neighbours p (b^.bounds)

fromInts' :: [[Int]] -> [[SquareState]]
fromInts' bbs = [[if b > 0 then Mine else Covered | b <- bs] | bs <- bbs]

fromInts :: [[Int]] -> Board
fromInts bs = Board { _squares = fromInts' bs, _bounds = bounds, _csp = newMinesweeper bounds 0}
  where bounds = (length bs, length $ bs !! 0)

type BoardState a = State Board a
click :: Pos -> BoardState ()
click p = do
  b <- get
  let sq = Square p (neighbouringMines b p)
  csp %= execMinesweeper (playMove sq)

  return ()
