module Game where

import Control.Monad

import Data.Maybe
import Data.Char
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Set as Set

import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import qualified CSP

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as:chunksOf n bs
  where
    (as, bs) = splitAt n xs

-- Squares can be:
--  - Covered without a mine or flag     (False False)
--  - Covered with a mine and unflagged  (True  False)
--  - Covered with a mine and flagged    (True  True)
--  - Covered without a mine and flagged (False True)
data Square
  = Covered Bool Bool
  | Uncovered
  deriving Show

hasMine :: Square -> Bool
hasMine (Covered m _) = m
hasMine _ = False

isFlagged :: Square -> Bool
isFlagged (Covered _ f) = f
isFlagged _ = False

uncovered :: Square -> Bool
uncovered Uncovered = True
uncovered _ = False

toggleFlag' :: Square -> Square
toggleFlag' (Covered m f) = Covered m (not f)

type Bounds = CSP.Pos

type Board = Array.Array CSP.Pos Square

bounds :: Board -> Bounds
bounds sqs = let (_, (rows, cols)) = Array.bounds sqs in (rows+1, cols+1)

fromBool :: Bool -> Square
fromBool m = Covered m False

fromBools :: [[Bool]] -> Board
fromBools bools = Array.array ((0, 0), bs) $ convert bools
  where
    row :: [Bool] -> [(Int, Square)]
    row = zip [0..] . map fromBool

    convert :: [[Bool]] -> [(CSP.Pos, Square)]
    convert = concat . map (\(x, row) -> map (\(y, s) -> ((x, y), s)) row) . zip [0..] . map row

    bs = (length bools - 1, length (head bools) - 1)

fromInts :: [[Int]] -> Board
fromInts = fromBools . map (map CSP.itob)

testBoard = fromInts [
    [0, 0, 1, 1, 1, 0]
  , [0, 0, 1, 0, 1, 0]
  , [0, 0, 1, 1, 1, 0]
  , [0, 0, 0, 0, 0, 0]
  , [1, 1, 0, 0, 0, 0]
  ]

genBoard :: RandomGen g => g -> Int -> Bounds -> (Board, g)
genBoard g n (rows, cols) = runRand (do
    bs <- shuffleM $ replicate n True ++ replicate (rows * cols - n) False
    return $ fromBools $ chunksOf cols bs
  ) g

toLists :: Board -> [[Square]]
toLists = map (map snd) . List.groupBy grouper . Array.assocs
  where
    grouper = \((x, _), _) ((x2, _), _) -> x == x2

squareAt :: CSP.Pos -> Board -> Square
squareAt p b = b Array.! p

neighbouringSquares :: CSP.Pos -> Board -> Set.Set CSP.Pos
neighbouringSquares p b = CSP.neighbours p (bounds b)

neighbouringMines :: CSP.Pos -> Board -> Set.Set CSP.Pos
neighbouringMines p b =
  Set.filter filterer (neighbouringSquares p b)
  where
    filterer = hasMine . flip squareAt b

neighbouringMineCount :: CSP.Pos -> Board -> Int
neighbouringMineCount p b =
  foldr reducer 0 (neighbouringSquares p b)
  where
    reducer = (+) . CSP.btoi . hasMine . flip squareAt b

charSquare :: CSP.Pos -> Board -> Char
charSquare p b =
  case sq of
    Covered False False -> '#'
    Covered True  False -> '*'
    Covered True  True  -> 'F'
    Covered False True  -> 'f'
    _ -> if
      ns > 0 then intToDigit ns
      else '1'
  where
    sq = squareAt p b
    ns = neighbouringMineCount p b

strRow' :: Board -> Int -> String
strRow' b x = [charSquare (x, y) b | y <- [0..snd (bounds b)-1]]

strBoard :: Board -> String
strBoard b = List.intercalate "\n" $ [strRow' b x | x <- [0..fst (bounds b)-1]]

setSquare :: CSP.Pos -> Square -> Board -> Board
setSquare p s b = b Array.// [(p, s)]

changeSquare :: (Square -> Square) -> CSP.Pos -> Board -> Board
changeSquare f p b = setSquare p (f $ squareAt p b) b

data Game = Game {
  board :: Board
, csp :: CSP.Minesweeper
, mistake :: Maybe CSP.Pos
, won :: Bool
} deriving (Show)

lost :: Game -> Bool
lost = isJust . mistake

changeBoard :: (Board -> Board) -> Game -> Game
changeBoard f g = g { board = f $ board g }

game :: Board -> Game
game b = Game { board = b, csp = CSP.newMinesweeper (bounds b) mineCount, mistake = Nothing, won = False }
  where
    mineCount = foldr ((+) . CSP.btoi . hasMine) 0 $ Array.elems b

genGame :: RandomGen g => g -> Int -> Bounds -> (Game, g)
genGame gen n bs = let (b, gen') = genBoard gen n bs in (game b, gen')

toggleFlag :: CSP.Pos -> Game -> Game
toggleFlag p g
  -- If the game is won / lost, do nothing
  | won g || lost g = g
  | otherwise = changeBoard (changeSquare toggleFlag' p) g

hasWon :: Game -> Bool
hasWon = all hasMine . filter (not . uncovered) . Array.elems . board

checkWon :: Game -> Game
checkWon g = g { won = hasWon g }

uncover :: CSP.Pos -> Game -> Game
uncover p g
  -- If the game is won / lost, do nothing
  | won g || lost g = g
  | otherwise =
    case sq of
      -- Hit an unflagged mine, that's a loss...
      Covered True  False -> g { mistake = Just p }
      Covered False False ->
        case neighbouringMineCount p b of
          -- Automatically uncover neighbours if there are no neighbouring mines
          0 -> foldr ($) uncovered uncoverNeighbours
          _ -> uncovered
      -- Do nothing if the square has already been uncovered or is flagged
      _ -> g
    where
      b = board g
      sq = squareAt p b

      uncovered = checkWon g { board = setSquare p Uncovered b }
      uncoverNeighbours = uncover <$> Set.toList (neighbouringSquares p b)
