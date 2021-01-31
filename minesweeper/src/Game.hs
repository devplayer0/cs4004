{-# LANGUAGE TemplateHaskell #-}
module Game where

import Control.Monad
import Control.Monad.State
import Control.Lens

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
    convert = concatMap (\(x, row) -> map (\(y, s) -> ((x, y), s)) row) . zip [0..] . map row

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

squareAt :: Board -> CSP.Pos -> Square
squareAt = (Array.!)

neighbouringSquares :: Board -> CSP.Pos -> Set.Set CSP.Pos
neighbouringSquares b = CSP.neighbours (bounds b)

neighbouringMines :: Board -> CSP.Pos -> Set.Set CSP.Pos
neighbouringMines b =
  Set.filter filterer . neighbouringSquares b
  where
    filterer = hasMine . squareAt b

neighbouringMineCount :: Board -> CSP.Pos -> Int
neighbouringMineCount b =
  foldr reducer 0 . neighbouringSquares b
  where
    reducer = (+) . CSP.btoi . hasMine . squareAt b

charSquare :: Board -> CSP.Pos -> Char
charSquare b p =
  case sq of
    Covered False False -> '#'
    Covered True  False -> '*'
    Covered True  True  -> 'F'
    Covered False True  -> 'f'
    _ -> if
      ns > 0 then intToDigit ns
      else '1'
  where
    sq = b `squareAt` p
    ns = neighbouringMineCount b p

strRow' :: Board -> Int -> String
strRow' b x = [charSquare b (x, y) | y <- [0..snd (bounds b)-1]]

strBoard :: Board -> String
strBoard b = List.intercalate "\n" $ [strRow' b x | x <- [0..fst (bounds b)-1]]

setSquare :: CSP.Pos -> Square -> Board -> Board
setSquare p s b = b Array.// [(p, s)]

changeSquare :: (Square -> Square) -> CSP.Pos -> Board -> Board
changeSquare f p b = setSquare p (f $ b `squareAt` p) b

data Game = Game {
  _board :: Board
, _mistake :: Maybe CSP.Pos
, _won :: Bool
} deriving (Show)
makeLenses  ''Game

lost :: Game -> Bool
lost = isJust . _mistake

changeBoard :: (Board -> Board) -> Game -> Game
changeBoard f g = g & board %~ f

game :: Board -> Game
game b = Game { _board = b, _mistake = Nothing, _won = False }
  where
    mineCount = foldr ((+) . CSP.btoi . hasMine) 0 $ Array.elems b

genGame :: RandomGen g => g -> Int -> Bounds -> (Game, g)
genGame gen n bs = let (b, gen') = genBoard gen n bs in (game b, gen')

toggleFlag :: CSP.Pos -> Game -> Game
toggleFlag p g
  -- If the game is won / lost, do nothing
  | _won g || lost g = g
  | otherwise = changeBoard (changeSquare toggleFlag' p) g

hasWon :: Game -> Bool
hasWon = all hasMine . filter (not . uncovered) . Array.elems . _board

checkWon :: Game -> Game
checkWon g = g & won .~ hasWon g

type GameState = State Game

execGame :: GameState a -> Game -> Game
execGame = execState

uncoverHook' :: GameState () -> CSP.Pos -> GameState ()
uncoverHook' hook p = do
  g <- get

  -- Only keep going if the game hasn't been won / lost
  unless (_won g || lost g) $
    case (g^.board) `squareAt` p of
      -- Hit an unflagged mine, that's a loss...
      Covered True  False -> mistake .= Just p
      Covered False False -> do
        board %= setSquare p Uncovered
        modify checkWon
        hook

        b <- use board
        when (b `neighbouringMineCount` p == 0) $
          -- Automatically uncover neighbours if there are no neighbouring mines
          mapM_ (uncoverHook' hook) $ Set.toList (b `neighbouringSquares` p)
      -- Do nothing if the square has already been uncovered or is flagged
      _ -> return ()

uncoverHook :: GameState () -> CSP.Pos -> Game -> Game
uncoverHook hook = execGame . uncoverHook' hook

uncover' :: CSP.Pos -> GameState()
uncover' = uncoverHook' (return ())

uncover :: CSP.Pos -> Game -> Game
uncover = execGame . uncover'
