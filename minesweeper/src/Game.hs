{-# LANGUAGE TemplateHaskell #-}
module Game where

import Control.Monad
import Control.Monad.Trans.State
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

import Debug.Trace

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
bounds sqs = let (_, (width, height)) = Array.bounds sqs in (width+1, height+1)

fromBool :: Bool -> Square
fromBool m = Covered m False

fromBools :: [[Bool]] -> Board
fromBools bools = Array.array ((0, 0), bs) $ convert bools
  where
    makeRow :: [Bool] -> [(Int, Square)]
    makeRow = zip [0..] . map fromBool

    convert :: [[Bool]] -> [(CSP.Pos, Square)]
    convert = concatMap (\(y, row) -> map (\(x, s) -> ((x, y), s)) row) . zip [0..] . map makeRow

    bs = (length (head bools) - 1, length bools - 1)

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
genBoard g n (width, height) = runRand (do
    bs <- shuffleM $ replicate n True ++ replicate (height * width - n) False
    return $ fromBools $ chunksOf width bs
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

setSquare :: CSP.Pos -> Square -> Board -> Board
setSquare p s b = b Array.// [(p, s)]

changeSquare :: (Square -> Square) -> CSP.Pos -> Board -> Board
changeSquare f p b = setSquare p (f $ b `squareAt` p) b

data GameState = GameState {
  _board :: Board
, _mistake :: Maybe CSP.Pos
, _won :: Bool
} deriving (Show)
makeLenses  ''GameState

lost :: GameState -> Bool
lost = isJust . _mistake

data Charset = Charset {
  sCovered :: String
, sUncovered :: Int -> String
, sFlag :: String
, sBadFlag :: String
, sMine :: String
, sLoseMine :: String
}

defaultCharset = Charset {
  sCovered   = "#"
, sUncovered = \n -> if n > 0 then show n else " "
, sFlag      = "F"
, sBadFlag   = "f"
, sMine      = "*"
, sLoseMine  = "X"
}

strSquare :: Charset -> CSP.Pos -> GameState -> String
strSquare cs p g = f' cs
  where
    f'
      | l && not m && f                 = sBadFlag
      | f || (w && m)                   = sFlag
      | l && fromJust (_mistake g) == p = sLoseMine
      | l && m                          = sMine
      | uc                              = (`sUncovered` ns)
      | otherwise                       = sCovered

    b = _board g
    l = lost g
    w = _won g

    sq = b `squareAt` p
    m = hasMine sq
    f = isFlagged sq
    uc = uncovered sq

    ns = neighbouringMineCount b p

strRow :: GameState -> Int -> String
strRow g r = concat [strSquare defaultCharset (x, r) g | x <- [0..fst (bounds $ _board g)-1]]

strBoard :: GameState -> String
strBoard g = List.intercalate "\n" $ [strRow g r | r <- [0..snd (bounds $ _board g)-1]]

strGame :: GameState -> String
strGame g = strBoard g ++ msg
  where
    msg
      | lost g    = "\nGame lost."
      | _won g    = "\nGame won!"
      | otherwise = ""

changeBoard :: (Board -> Board) -> GameState -> GameState
changeBoard f g = g & board %~ f

game :: Board -> GameState
game b = GameState { _board = b, _mistake = Nothing, _won = False }
  where
    mineCount = foldr ((+) . CSP.btoi . hasMine) 0 $ Array.elems b

genGame :: RandomGen g => g -> Int -> Bounds -> (GameState, g)
genGame gen n bs = let (b, gen') = genBoard gen n bs in (game b, gen')

toggleFlag :: CSP.Pos -> GameState -> GameState
--toggleFlag p g | trace ("toggleFlag "++show p) False = undefined
toggleFlag p g
  -- If the game is won / lost, do nothing
  | _won g || lost g = g
  | otherwise = changeBoard (changeSquare toggleFlag' p) g

hasWon :: GameState -> Bool
hasWon = all hasMine . filter (not . uncovered) . Array.elems . _board

checkWon :: GameState -> GameState
checkWon g = g & won .~ hasWon g

type GameT = StateT GameState
type Game = StateT GameState Identity

execGameT :: Monad m => GameT m a -> GameState -> m GameState
execGameT = execStateT

runGameT :: Monad m => GameT m a -> GameState -> m (a, GameState)
runGameT = runStateT

execGame :: Game a -> GameState -> GameState
execGame = execState

type UncoverHook m = CSP.Pos -> GameState -> m ()

uncoverHookT :: Monad m => UncoverHook m -> CSP.Pos -> GameT m ()
uncoverHookT hook p = do
  g <- get

  -- Only keep going if the game hasn't been won / lost
  unless (_won g || lost g) $
    case (g^.board) `squareAt` p of
      -- Hit an unflagged mine, that's a loss...
      Covered True  False -> mistake .= Just p
      Covered False False -> do
        board %= setSquare p Uncovered
        modify checkWon
        get >>= lift . hook p

        b <- gets _board
        when (b `neighbouringMineCount` p == 0) $
          -- Automatically uncover neighbours if there are no neighbouring mines
          mapM_ (uncoverHookT hook) $ Set.toList (b `neighbouringSquares` p)
      -- Do nothing if the square has already been uncovered or is flagged
      _ -> return ()

uncoverHook :: Monad m => UncoverHook m -> CSP.Pos -> GameState -> m GameState
uncoverHook hook = execGameT . uncoverHookT hook

uncover' :: CSP.Pos -> Game ()
uncover' = uncoverHookT (\_ _ -> return ())

uncover :: CSP.Pos -> GameState -> GameState
uncover = execGame . uncover'

-- Game + CSP solver

type CSPGameState = (GameState, CSP.CSPState)
cspGame :: Board -> CSPGameState
cspGame b = (game b, CSP.newMinesweeper (bounds b) mineCount)
  where
    mineCount = foldr ((+) . CSP.btoi . hasMine) 0 $ Array.elems b

genCSPGame :: RandomGen g => g -> Int -> Bounds -> (CSPGameState, g)
genCSPGame gen n bs = let (b, gen') = genBoard gen n bs in (cspGame b, gen')

genCSPGamesForever :: RandomGen g => g -> Int -> Bounds -> [CSPGameState]
genCSPGamesForever gen n bs = g:genCSPGamesForever nextGen n bs
  where
    (g, nextGen) = genCSPGame gen n bs

cspHook :: CSP.Pos -> GameState -> CSP.CSP ()
cspHook p g = CSP.playMove' (CSP.Square p ns)
  where ns = _board g `neighbouringMineCount` p

uncoverCSP' :: CSP.Pos -> GameT CSP.CSP [CSP.Move]
uncoverCSP' p = do
  -- Do all the uncovering
  uncoverHookT cspHook p

  (moves, mines) <- lift $ gets CSP.moves

  -- Flag any mines and push them into the state
  mapM_ (\p -> do
    modify $ toggleFlag p
    lift $ CSP.addMine' p
    ) mines

  return moves

-- uncoverCSP uncovers a mine
uncoverCSP :: CSP.Pos -> CSPGameState -> ([CSP.Move], CSPGameState)
uncoverCSP p s = (moves, (g, c))
  where
    ((moves, g), c) = uncurry (CSP.runMinesweeper  . runGameT (uncoverCSP' p)) s

aiPlayNext :: CSPGameState -> CSPGameState
aiPlayNext cspG = uncurry (CSP.runMinesweeper . execGameT playNext) cspG
  where
    (g, c) = cspG

    playNext = do
      (moves, mines) <- lift $ gets CSP.moves

      mapM_ (\p -> do
        modify $ toggleFlag p
        lift $ CSP.addMine' p
        ) mines

      let
        move = head moves
        --move  = trace (show c ++ strGame g ++ "\nPlaying " ++ show move') move'
      unless (List.null moves) . void . uncoverCSP' $ CSP._movePos move

aiPlayThrough :: CSPGameState -> CSPGameState
aiPlayThrough cspG
  | _won g || lost g = cspG
  | otherwise        = aiPlayThrough $ aiPlayNext cspG
  where
    (g, _) = cspG

-- Generate a number of games and run the solver over each until they are either
-- won / lost, returning the fraction of won games
aiMeasureSuccess :: Int -> Int -> Bounds -> Float
aiMeasureSuccess n nms bs = CSP.sumWith (fromIntegral . CSP.btoi . _won . fst) games / fromIntegral n
  where
    games = take n . map aiPlayThrough $ genCSPGamesForever (mkStdGen 1337) nms bs

changeGame :: (GameState -> GameState) -> CSPGameState -> CSPGameState
changeGame f (g, c) = (f g, c)
