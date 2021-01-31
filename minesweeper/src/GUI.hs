{-# LANGUAGE RecursiveDo #-}
module GUI where

import Control.Monad

import Data.Tuple
import Data.Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import Data.IORef
import Text.Printf

import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified CSP
import Game

import Debug.Trace

guiCellCharset = Charset {
  sCovered = " "
, sUncovered = \n -> if n > 0 then show n else " "
, sFlag = "🚩"
, sBadFlag = "🏁"
, sMine = "💣"
, sLoseMine = "🧨"
}

cellEnabled :: Square -> Bool
cellEnabled Uncovered = False
cellEnabled _         = True

percentage :: Float -> Int
percentage = round . (*) 100

type HSL = (Int, Float, Float)

probToHSL :: Float -> HSL
probToHSL p = (round $ 125 * (1 - p), 0.75, 0.5)

hslStr :: HSL -> String
hslStr (h, s, l) = printf "hsl(%d, %d%%, %d%%)" h (percentage s) (percentage l)

cellColor :: Square -> Maybe CSP.Move -> String
cellColor Uncovered _ = "white"
cellColor _ Nothing   = "white"
cellColor sq (Just (CSP.Move CSP.KnownVariable _ _)) = "blue"
cellColor sq (Just m) = hslStr . probToHSL $ CSP._mineProbability m

cellStyle :: Square -> Maybe CSP.Move -> [(String, String)]
cellStyle sq m = [("background-color", cellColor sq m)]

data GUIState = GUIState {
  aiGame :: CSPGameState
, moves :: [CSP.Move]
}

game_ :: GUIState -> Game.GameState
game_ = fst . aiGame

csp :: GUIState -> CSP.CSPState
csp = snd . aiGame

sTuple :: ([CSP.Move], CSPGameState) -> GUIState
sTuple (ms, s') = GUIState s' ms

changeCSPGame :: (CSPGameState -> CSPGameState) -> GUIState -> GUIState
changeCSPGame f s = s { aiGame = f $ aiGame s }

changeGame_ :: (GameState -> GameState) -> GUIState -> GUIState
changeGame_ = changeCSPGame . changeGame

makeCell :: Behavior GUIState -> CSP.Pos -> UI (Element, Event (GUIState  -> GUIState))
makeCell bGame p = do
  cell <- UI.button #. "cell"

  -- hack so that we can receive key press events
  on UI.hover cell $ \_ -> UI.setFocus cell

  let
    eUncover = UI.click cell
    eFlag    = filterE ('f' ==) $ UI.keypress cell

  let
    events = concatenate <$> unions [
        sTuple . uncoverCSP p . aiGame <$ eUncover
      , changeGame_ (toggleFlag p) <$ eFlag
      ]

  let
    bBoard = _board . game_ <$> bGame
    bSquare = (`squareAt` p) <$> bBoard

    fSq = (`squareAt` p) . _board . game_
    fMove = List.find ((== p) . CSP._movePos) . moves
    bStyleInfo = (\g -> (fSq g, fMove g)) <$> bGame

    bText = strSquare guiCellCharset p . game_ <$> bGame
    bEnabled = cellEnabled . (`squareAt` p)  <$> bBoard
    bStyle = uncurry cellStyle <$> bStyleInfo

  element cell
    # sink UI.text    bText
    # sink UI.enabled bEnabled
    # sink UI.style   bStyle

  return (cell, events)

newGame :: UI ()
newGame = runFunction $ ffi "location.reload()"

setup :: RandomGen g => IORef g -> Window -> UI ()
setup genRef window = void $ mdo
  g' <- liftIO $ atomicModifyIORef genRef (\gen -> swap $ genCSPGame gen 10 (9, 9))
  let g_ = GUIState g' (fst . CSP.moves $ snd g')
  let g = trace ("moves " ++ show (moves g_)) g_

  return window # set title "Minesweeper"
  UI.addStyleSheet window "styles.css"

  let positions = List.groupBy (\(x, _) (x2, _) -> x == x2) $ Array.indices (_board $ game_ g)
  cells' <- mapM (mapM $ makeCell bGame) positions
  let
    cells = (<$>) (element . fst) <$> cells'
    events = snd <$> concat cells'

  message <- UI.paragraph
  let
    msgText g
      | _won g    = "You win!"
      | lost g    = "You lose."
      | otherwise = ""
    bMsgText = msgText . game_ <$> bGame

  element message #
    sink UI.text bMsgText

  btnNewGame <- UI.button #. "newGame"
  on UI.click btnNewGame $ const newGame
  element btnNewGame #
    set UI.text "New game"

  body <- getBody window
  element body #+ [
      grid cells
    , element btnNewGame
    , element message
    ]

  bGame <- accumB g $ concatenate <$> unions events

  return ()

run :: IO ()
run = do
  gen <- getStdGen
  gRef <- newIORef gen
  startGUI defaultConfig { jsStatic = Just "static" } (setup gRef)
